open Utils

(* Variables scope *)
type scope =
  { vars : Vm.nameid Name_map.t
  ; var_list : Vm.nameid list
  ; outer : scope option
  }

let root_scope =
  { vars = Name_map.empty
  ; var_list = []
  ; outer = None
  }

let new_scope outer =
  { vars = Name_map.empty
  ; var_list = []
  ; outer = Some outer
  }
      
(* Local environment *)
type env =
  { mutable capts : Vm.nameid Name_map.t
  ; mutable capt_list : Vm.nameid list
  ; captgen : idgen
  ; vargen : idgen
  ; lblgen : idgen
  }

let new_env () =
  { capts = Name_map.empty
  ; capt_list = []
  ; captgen = make_idgen 0
  ; vargen = make_idgen 0
  ; lblgen = make_idgen 0
  }

let new_capt env ~prefix =
  let captid = env.captgen.new_id () in
  let name = prefix ^ "_" ^ string_of_int captid in
  captid, name

let new_var env ~prefix =
  let varid = env.vargen.new_id () in
  let name = prefix ^ "_" ^ string_of_int varid in
  varid, name

let new_label env ~prefix =
  "." ^ prefix ^ string_of_int (env.lblgen.new_id ())

let add_capt env name =
  let capts = env.capts in
  let captid = env.captgen.new_id () in
  let nameid = (captid, name) in
  let capts = Name_map.add name nameid capts in
  let capt_list = nameid :: env.capt_list in
  env.capts <- capts;
  env.capt_list <- capt_list;
  nameid

let get_captured_vars env =
  (* let capts = List.of_seq (Name_map.to_seq env.capts) in
  let _, capts = List.split capts in *)
  let capts = env.capt_list in
  List.sort compare capts

let get_number_of_vars env =
  env.vargen.current_id ()

type globenv =
  { funcgen : idgen
  ; gvargen : idgen
  }

let new_globenv () =
  { funcgen = make_idgen 0
  ; gvargen = make_idgen 0
  }

let new_func globenv ~prefix =
  let funcid = globenv.funcgen.new_id () in
  let name = "%" ^ prefix ^ "_" ^ string_of_int funcid in
  funcid, name


let add_to_scope ({vars; var_list} as scope) env name =
  let varid = env.vargen.new_id () in
  let nameid = (varid, name) in
  let vars = Name_map.add name nameid vars in
  let var_list = nameid :: var_list in
  nameid, { scope with vars; var_list }

type location =
  | Null
  | Outer of Vm.nameid
  | Inner of Vm.nameid
  | Global of Vm.nameid

let rec lookup_scope scope name = 
  let vars = scope.vars in
  match Name_map.find_opt name vars with
  | Some x ->
    (match scope.outer with
    | None -> Global x
    | Some _ -> Inner x
    )
  | None ->
    match scope.outer with
    | None -> Null
    | Some outer -> 
      match lookup_scope outer name with
      | Null -> Null
      | Outer x | Inner x -> Outer x
      | Global x -> Global x


exception Righthand_side_disallowed

let lookup_var scope env name =
  match Name_map.find_opt name env.capts with
  | Some x -> Vm.Outer x
  | None ->
    match lookup_scope scope name with
    | Inner x -> Vm.Local x
    | Global x -> Vm.Global x
    | Outer x ->
      let nameid = add_capt env name in
      Vm.Outer nameid
    | Null ->
      let x = Name_map.find name Vm.externals in
      Vm.External x

let add_global globenv scope (varid, name as nameid) =
  let vars = scope.vars in
  let vars = Name_map.add name nameid vars in
  let var_list = nameid :: scope.var_list in
  { scope with vars; var_list }

let gen_globals globenv {var_list} =
  let var_list = List.rev var_list in
  let map (id, name) =
    let varid = globenv.gvargen.new_id () in
    (varid, name), (id, name)
  in
  List.map map var_list

let get_vars {var_list} =
  List.sort compare var_list

(* Code generation *)

type output =
  { funcs : Vm.func list
  ; proc : Vm.proc
  }

let empty_output =
  {funcs=[]; proc=[]}

let concat_outputs o1 o2 =
  { funcs = o1.funcs @ o2.funcs
  ; proc = o1.proc @ o2.proc
  }

let add_names scope env names =
  let iter (scope, nameids) name =
    let nameid, scope = add_to_scope scope env name in
    scope, (nameid :: nameids)
  in
  let scope, nameids = List.fold_left iter (scope, []) names in
  let nameids = List.map (fun (n, x) -> (x, (n, x))) nameids in
  scope, Name_map.of_seq (List.to_seq nameids)

let rec walk_value_bindings orig_scope env globenv (r, l) =
  let failure_label = new_label env ~prefix:"L" in
  let failure_target = Vm.make_target failure_label in
  let failure_proc =
    let after_label = new_label env ~prefix:"L" in
    let after_target = Vm.make_target after_label in
    let jmp = Vm.J (Vm.Jmp, after_target) in
    let label = Vm.L failure_label in
    let crash = Vm.S (Vm.Crash) in
    let after = Vm.L (after_label) in
    [jmp; label; crash; after]
  in
  if r then
    let iter_pattern (scope, acc_outputs, acc_src_vars) (names, p, _) =
      let scope, nameids = add_names scope env names in
      let src_var = Vm.Local (new_var env ~prefix:"%var") in
      let output = walk_pattern env nameids src_var failure_target p in
      scope, (output::acc_outputs), (src_var::acc_src_vars)
    in
    let scope, pattern_outputs, src_vars =
      List.fold_left iter_pattern (orig_scope,[],[]) l in
    let outputs = List.rev pattern_outputs in
    let srcs = List.rev src_vars in
    let lst = List.combine l (List.combine outputs srcs) in
    let iter_expr acc_outputs ((_, _, e), (pattern_output, dest_var)) =
      let expr_output = walk_expr scope env globenv dest_var e in
      let proc = expr_output.proc @ pattern_output.proc in
      let funcs = pattern_output.funcs @ expr_output.funcs in
      let output = {funcs; proc} in
      output :: acc_outputs
    in
    let outputs = List.fold_left iter_expr [] lst in
    let output = List.fold_right concat_outputs (List.rev outputs) empty_output in
    let proc = output.proc @ failure_proc in
    let output = {output with proc} in
    scope, output
  else
    let iter (scope, acc_outputs) (names, p, e) =
      let chan_var = Vm.Local (new_var env ~prefix:"%var") in
      let expr_output = walk_expr scope env globenv chan_var e in
      let scope, nameids = add_names scope env names in
      let pattern_output = walk_pattern env nameids chan_var failure_target p in
      let output = concat_outputs expr_output pattern_output in
      scope, (output :: acc_outputs)
    in
    let scope, outputs = List.fold_left iter (orig_scope, []) l in
    let output = List.fold_right concat_outputs (List.rev outputs) empty_output in
    let proc = output.proc @ failure_proc in
    let output = {output with proc} in
    scope, output

and walk_pattern env nameids src_var failure_target = function
  | Typed_ast.UnitPattern _ ->
    let proc =
      let var = Vm.Local (new_var env ~prefix:"%var") in
      let load = Vm.V (Vm.LoadUnit, var) in
      let cmp = Vm.VVV (Vm.Eq, Vm.Wildcard, var, src_var) in
      let jmp = Vm.J (Vm.JmpFalse, failure_target) in
      [load; cmp; jmp]
    in
    {funcs=[]; proc}
    
  | Typed_ast.BoolPattern (b, _) -> 
    let proc =
      let var = Vm.Local (new_var env ~prefix:"%var") in
      let load = Vm.VB (Vm.LoadBool, var, b) in
      let cmp = Vm.VVV (Vm.Eq, Vm.Wildcard, var, src_var) in
      let jmp = Vm.J (Vm.JmpFalse, failure_target) in
      [load; cmp; jmp]
    in
    {funcs=[]; proc}

  | Typed_ast.IntPattern (i, _) ->
    let proc =
      let var = Vm.Local (new_var env ~prefix:"%var") in
      let load = Vm.VI (Vm.LoadInt, var, i) in
      let cmp = Vm.VVV (Vm.Eq, Vm.Wildcard, var, src_var) in
      let jmp = Vm.J (Vm.JmpFalse, failure_target) in
      [load; cmp; jmp]
    in
    {funcs=[]; proc}

  | Typed_ast.CharPattern (c, _) ->
    let proc =
      let var = Vm.Local (new_var env ~prefix:"%var") in
      let load = Vm.VC (Vm.LoadChar, var, c) in
      let cmp = Vm.VVV (Vm.Eq, Vm.Wildcard, var, src_var) in
      let jmp = Vm.J (Vm.JmpFalse, failure_target) in
      [load; cmp; jmp]
    in
    {funcs=[]; proc}
    
  | Typed_ast.StringPattern (s, _) ->
    let proc =
      let var = Vm.Local (new_var env ~prefix:"%var") in
      let load = Vm.VS (Vm.LoadStr, var, s) in
      let cmp = Vm.VVV (Vm.Eq, Vm.Wildcard, var, src_var) in
      let jmp = Vm.J (Vm.JmpFalse, failure_target) in
      [load; cmp; jmp]
    in
    {funcs=[]; proc}

  | Typed_ast.TuplePattern (l, _) ->
    let iter (acc_outputs, idx) e =
      let part_var = Vm.Local (new_var env ~prefix:"%var") in
      let part_output = walk_pattern env nameids part_var failure_target e in
      let proc =
        let part = Vm.VVI (Vm.TuplePart, part_var, src_var, idx) in
        part :: part_output.proc
      in
      let output = {funcs=[]; proc} in
      output :: acc_outputs, idx + 1
    in
    let outputs, _ = List.fold_left iter ([], 0) l in
    let output = List.fold_left concat_outputs empty_output (List.rev outputs) in
    output
  
  | Typed_ast.ListPattern (l, t) ->
    let ast = match l with
      | [] -> Typed_ast.VariantPattern (Types.list_nil, None, t)
      | p :: rest ->
        let rest = Typed_ast.ListPattern (rest, t) in
        Typed_ast.ConsPattern (p, rest, t)
    in
    walk_pattern env nameids src_var failure_target ast

  | Typed_ast.ConsPattern (p, l, t) ->
    let param_t = List.hd (Types.extract_param_type t) in
    let sub_p = Typed_ast.TuplePattern ([p; l], Types.Tuple [param_t; t]) in
    let ast = Typed_ast.VariantPattern (Types.list_cons, Some sub_p, t) in
    walk_pattern env nameids src_var failure_target ast

  | Typed_ast.ArrayPattern (l, _) ->
    failwith "walk_pattern"

  | Typed_ast.RecordPattern (l, _) ->
    failwith "walk_pattern"

  | Typed_ast.VariablePattern (name, _) ->
    let nameid = Name_map.find name nameids in
    let proc =
      let var = Vm.Local nameid in
      let move = Vm.VV (Vm.Move, var, src_var) in
      [move]
    in
    {funcs=[]; proc}

  | Typed_ast.Wildcard _ ->
    empty_output

  | Typed_ast.RefPattern (p, _) ->
    let var = Vm.Local (new_var env ~prefix:"%var") in
    let output = walk_pattern env nameids var failure_target p in
    let proc = output.proc @ [Vm.VV (Vm.Deref, var, src_var)] in
    {funcs=[]; proc}

  | Typed_ast.VariantPattern ((i, _, _), None, _) ->
    let num_var = Vm.Local (new_var env ~prefix:"%var") in
    let proc =
      let vrnum = Vm.VV (Vm.VariantNum, num_var, src_var) in
      let tmp_var = Vm.Local (new_var env ~prefix:"%var") in
      let load = Vm.VI (Vm.LoadInt, tmp_var, i) in
      let cmp = Vm.VVV (Vm.Eq, Vm.Wildcard, num_var, tmp_var) in
      let jmp = Vm.J (Vm.JmpFalse, failure_target) in
      [vrnum; load; cmp; jmp]
    in
    {funcs=[]; proc}

  | Typed_ast.VariantPattern ((i, _, _), Some p, _) ->
    let num_var = Vm.Local (new_var env ~prefix:"%var") in
    let val_var = Vm.Local (new_var env ~prefix:"%var") in
    let output = walk_pattern env nameids val_var failure_target p in
    let proc =
      let vrnum = Vm.VV (Vm.VariantNum, num_var, src_var) in
      let tmp_var = Vm.Local (new_var env ~prefix:"%var") in
      let load = Vm.VI (Vm.LoadInt, tmp_var, i) in
      let cmp = Vm.VVV (Vm.Eq, Vm.Wildcard, num_var, tmp_var) in
      let jmp = Vm.J (Vm.JmpFalse, failure_target) in
      let vrval = Vm.VV (Vm.VariantVal, val_var, src_var) in
      [vrnum; load; cmp; jmp; vrval] @ output.proc
    in
    {funcs=[]; proc}

  | Typed_ast.PatternList (l, _) ->
    let rec iter acc_outputs = function
      | [] -> failwith "walk_pattern: pattern_list"
      | e :: [] ->
        let output = walk_pattern env nameids src_var failure_target e in
        (output :: acc_outputs)
      | e :: l ->
        let next_label = new_label env ~prefix:"L" in
        let next_target = Vm.make_target next_label in
        let output = walk_pattern env nameids src_var next_target e in
        let proc =
          let label = Vm.L next_label in
          output.proc @ [label]
        in
        let output = {funcs=[]; proc} in
        iter (output :: acc_outputs) l
    in
    let outputs = List.rev (iter [] l) in
    let output = List.fold_right concat_outputs outputs empty_output in
    output
  
  | Typed_ast.PatternWithType (p, _) ->
    walk_pattern env nameids src_var failure_target p
    
and walk_expr scope env globenv dest_var = function
  | Typed_ast.Unit _ ->
    let proc = [Vm.V (Vm.LoadUnit, dest_var)] in
    {funcs=[]; proc}

  | Typed_ast.Bool (b, _) ->
    let proc = [Vm.VB (Vm.LoadBool, dest_var, b)] in
    {funcs=[]; proc}

  | Typed_ast.Int (i, _) ->
    let proc = [Vm.VI (Vm.LoadInt, dest_var, i)] in
    {funcs=[]; proc}

  | Typed_ast.Char (c, _) ->
    let proc = [Vm.VC (Vm.LoadChar, dest_var, c)] in
    {funcs=[]; proc}

  | Typed_ast.String (s, _) ->
    let proc = [Vm.VS (Vm.LoadStr, dest_var, s)] in
    {funcs=[]; proc}

  | Typed_ast.Variant ((i, _, _), _) ->
    let proc =
      let setnum = Vm.I (Vm.SetNum, i) in
      let makevr = Vm.V (Vm.MakeVariant, dest_var) in
      [setnum; makevr]
    in
    {funcs=[]; proc}

  | Typed_ast.Construct ((i, _, _), e, _) ->
    let var = Vm.Local (new_var env ~prefix:"%var") in
    let output = walk_expr scope env globenv var e in
    let proc =
      let setnum = Vm.I (Vm.SetNum, i) in
      let setval = Vm.V (Vm.SetVal, var) in
      let makevr = Vm.V (Vm.MakeVariant, dest_var) in
      [setnum; setval; makevr]
    in
    let output = concat_outputs output {funcs=[]; proc} in
    output

  | Typed_ast.Variable (_, name, _) ->
    let src_var = lookup_var scope env name in
    let proc =
      let move = Vm.VV (Vm.Move, dest_var, src_var) in [move]
    in
    {funcs=[]; proc}

  | Typed_ast.Assign (assignee, assigner, _) ->
    let dest = Vm.Local (new_var env ~prefix:"%var") in
    let src = Vm.Local (new_var env ~prefix:"%var") in
    let assignee_output = walk_expr scope env globenv dest assignee in
    let assigner_output = walk_expr scope env globenv src assigner in
    let proc =
      let assign = Vm.VV (Vm.SetRef, dest, src) in
      let load = Vm.V (Vm.LoadUnit, dest_var) in
      [assign; load]
    in
    let output = concat_outputs assignee_output assigner_output in
    let output = concat_outputs output {funcs=[]; proc} in
    output

  | Typed_ast.Tuple (l, _) ->
    let iter (env, acc_outputs, acc_vars) e =
      let var = Vm.Local (new_var env ~prefix:"%var") in
      let output = walk_expr scope env globenv var e in 
      env, (output :: acc_outputs), (var :: acc_vars)
    in
    let env, outputs, vars = List.fold_left iter (env, [], []) l in
    (* set tuple parts reversely *)
    let proc =
      let setparts = List.map (fun v -> Vm.V (Vm.PushPart, v)) vars in
      let maketuple = Vm.V (Vm.MakeTuple, dest_var) in
      setparts @ [maketuple]
    in
    let output = List.fold_left concat_outputs empty_output (List.rev outputs) in
    let output = concat_outputs output {funcs=[]; proc} in
    output

  | Typed_ast.List (l, t) ->
    (match l with
    | [] ->
      let variant = Typed_ast.Variant (Types.list_nil, t) in
      walk_expr scope env globenv dest_var variant
    | e :: l ->
      (* let item_t = match t with
        | Types.Specific ([typ], _) -> typ
        | _ -> failwith "walk_expr"
      in *)
      let item_t = List.hd (Types.extract_param_type t) in
      let ast = Typed_ast.Construct (Types.list_cons,
        Typed_ast.Tuple ([e; Typed_ast.List (l, t)], Types.Tuple [item_t; t]), t) in
      walk_expr scope env globenv dest_var ast
    )

  | Typed_ast.Array _ ->
    failwith "walk_expr: array"

  | Typed_ast.Record _ ->
    failwith "walk_expr: record"
  
  | Typed_ast.Call (clr, cle, _) ->
    let caller = Vm.Local (new_var env ~prefix:"%var") in
    let callee = Vm.Local (new_var env ~prefix:"%var") in
    let caller_output = walk_expr scope env globenv caller clr in
    let callee_output = walk_expr scope env globenv callee cle in
    let proc =
      let setarg = Vm.V (Vm.SetArg, callee) in
      let call = Vm.V (Vm.Call, caller) in
      let getret = Vm.V (Vm.GetRet, dest_var) in
      [setarg; call; getret]
    in
    let output = concat_outputs caller_output callee_output in
    let output = {output with proc=output.proc@proc} in
    output
  
  | Typed_ast.Unary (op, operand, _) ->
    let var = Vm.Local (new_var env ~prefix:"%var") in
    let output = walk_expr scope env globenv var operand in
    let proc = match op with
      | Ast.Positive -> [Vm.VV (Vm.Move, dest_var, var)]
      | Ast.Negative -> [Vm.VV (Vm.IntNeg, dest_var, var)]
      | Ast.Deref -> [Vm.VV (Vm.Deref, dest_var, var)]
    in
    let output = {output with proc = output.proc @ proc} in
    output

  | Typed_ast.Binary (op, v1, v2, _) ->
    let va = Vm.Local (new_var env ~prefix:"%var") in
    let vb = Vm.Local (new_var env ~prefix:"%var") in
    let a_output = walk_expr scope env globenv va v1 in
    let b_output = walk_expr scope env globenv vb v2 in
    let proc = match op with
      | Ast.Plus -> [Vm.VVV (Vm.IntAdd, dest_var, va, vb)]
      | Ast.Minus -> [Vm.VVV (Vm.IntSub , dest_var, va, vb)]
      | Ast.Times -> [Vm.VVV (Vm.IntMul , dest_var, va, vb)]
      | Ast.Div -> [Vm.VVV (Vm.IntDiv , dest_var, va, vb)]
      | Ast.Mod -> [Vm.VVV (Vm.IntMod , dest_var, va, vb)]
      | Ast.Lt -> [Vm.VVV (Vm.Lt, dest_var, va, vb)]
      | Ast.Lte -> [Vm.VVV (Vm.Lte, dest_var, va, vb)]
      | Ast.Gt -> [Vm.VVV (Vm.Gt, dest_var, va, vb)]
      | Ast.Gte -> [Vm.VVV (Vm.Gte, dest_var, va, vb)]
      | Ast.Eq -> [Vm.VVV (Vm.Eq, dest_var, va, vb)]
      | Ast.Neq -> [Vm.VVV (Vm.Neq, dest_var, va, vb)]
      | Ast.And -> [Vm.VVV (Vm.BoolAnd, dest_var, va, vb)]
      | Ast.Or -> [Vm.VVV (Vm.BoolOr, dest_var, va, vb)]
      | Ast.Cons -> failwith "walk_expr: cons"
      | Ast.Append -> failwith "walk_expr: append"
      | Ast.Concat -> [Vm.VVV (Vm.StrCat, dest_var, va, vb)]
    in
    let output = concat_outputs a_output b_output in
    let output = {output with proc = output.proc @ proc} in
    output

  | Typed_ast.Local (b, e, _) ->
    let scope, bindings_output = walk_value_bindings scope env globenv b in
    let expr_output = walk_expr scope env globenv dest_var e in
    let output = concat_outputs bindings_output expr_output in
    output

  | Typed_ast.IfExpr if_expr ->
    walk_if_expr scope env globenv dest_var if_expr

  | Typed_ast.MatchExpr match_expr ->
    walk_match_expr scope env globenv dest_var match_expr

  | Typed_ast.LambdaExpr lambda_expr ->
    walk_lambda_expr scope env globenv dest_var lambda_expr

  | Typed_ast.ExprList (l, _) ->
    let rec iter acc_outputs = function
      | [] -> acc_outputs
      | e :: [] ->
        let output = walk_expr scope env globenv dest_var e in
        (output :: acc_outputs)
      | e :: l ->
        let output = walk_expr scope env globenv Vm.Wildcard e in
        iter (output :: acc_outputs) l
    in
    let outputs = List.rev (iter [] l) in
    let output = List.fold_right concat_outputs outputs empty_output in
    output

  | Typed_ast.ExprWithType (e, _) ->
    walk_expr scope env globenv dest_var e

and walk_if_expr scope env globenv dest_var (cond_e, then_e, else_opt, _) =
  let after_label, after_proc, after_target =
    let label = new_label env ~prefix:"L" in
    label, [Vm.L label], Vm.{label; pos=[]}
  in
  let cond = Vm.Local (new_var env ~prefix:"%var") in
  let cond_output = walk_expr scope env globenv cond cond_e in
  let then_output = walk_expr scope env globenv dest_var then_e in
  let else_output = match else_opt with
    | None ->
      let proc = [Vm.V (Vm.LoadUnit, dest_var)] in
      {funcs=[]; proc}
    | Some else_e ->
      walk_expr scope env globenv dest_var else_e
  in
  let else_label = new_label env ~prefix:"L" in
  let else_target = Vm.{label=else_label; pos=[]} in
  let proc =
    let select_by_cond =
      let var = Vm.Local (new_var env ~prefix:"%var") in
      let load = Vm.VB (Vm.LoadBool, var, true) in
      let cmp = Vm.VVV (Vm.Eq, Vm.Wildcard, cond, var) in
      let jmp_to_else = Vm.J (Vm.JmpFalse, else_target) in
      cond_output.proc @ [load; cmp; jmp_to_else]
    in
    let then_proc =
      let jmp_after = Vm.J (Vm.Jmp, after_target) in
      then_output.proc @ [jmp_after]
    in
    let else_proc =
      let label = Vm.L else_label in
      label :: else_output.proc
    in
    select_by_cond @ then_proc @ else_proc @ after_proc
  in
  { funcs = cond_output.funcs @ then_output.funcs @ else_output.funcs
  ; proc = proc
  }

and walk_match_expr scope env globenv dest_var (e, l, _) =
  (* source expression *)
  let src_var = Vm.Local (new_var env ~prefix:"%var") in
  let src_output = walk_expr scope env globenv src_var e in
  (* pos after match expr *)
  let after_label = new_label env ~prefix:"L" in
  let after_target = Vm.make_target after_label in
  (* iterate each matching branch *)
  let iter acc_outputs (names, pattern, expr, _) =
    let next_label = new_label env ~prefix:"L" in
    let next_target = Vm.{label=next_label; pos=[]} in
    let scope, nameids = add_names scope env names in
    let pattern_output = walk_pattern env nameids src_var next_target pattern in
    let expr_output = walk_expr scope env globenv dest_var expr in
    let proc =
      let jmp_after = Vm.J (Vm.Jmp, after_target) in
      let next_label = Vm.L next_label in
      pattern_output.proc @ expr_output.proc @ [jmp_after; next_label]
    in
    let output =
      { funcs = pattern_output.funcs @ expr_output.funcs
      ; proc = proc
      } in
    output :: acc_outputs
  in
  let branch_outputs = List.rev (List.fold_left iter [] l) in
  let output =
    concat_outputs src_output (List.fold_right concat_outputs branch_outputs empty_output)
  in
  let proc =
    let crash = Vm.S (Vm.Crash) in
    let label = Vm.L after_label in
    [crash; label]
  in
  let output = {output with proc = output.proc @ proc} in
  output

and walk_lambda_expr scope env globenv dest_var (names, pattern, expr, _) =
  let funcid = new_func globenv ~prefix:"fun" in
  let func, funcs, captured =
    let scope = new_scope scope in
    let env = new_env () in
    let scope, nameids = add_names scope env names in
    let failure_label = new_label env ~prefix:"L" in
    let failure_target = Vm.make_target failure_label in
    let arg_var = Vm.Local (new_var env ~prefix:"%var") in
    let pattern_output = walk_pattern env nameids arg_var failure_target pattern in
    let ret_var = Vm.Local (new_var env ~prefix:"%var") in
    let expr_output = walk_expr scope env globenv ret_var expr in
    let proc =
      let getarg = Vm.V (Vm.GetArg, arg_var) in
      let setret = Vm.V (Vm.SetRet, ret_var) in
      let ret = Vm.S (Vm.Ret) in
      getarg :: pattern_output.proc @ expr_output.proc @ [setret; ret]
    in
    let captured = Array.of_list (get_captured_vars env) in
    let nvars = get_number_of_vars env in
    let funcs = pattern_output.funcs @ expr_output.funcs in
    Vm.{id=funcid; captured; nvars; proc}, funcs, captured
  in
  let id, name = funcid in
  let proc = 
    let makefunc = Vm.D (Vm.MakeFunc, (id, name), dest_var) in
    let rec iter proc i =
      if i = Array.length captured then proc else
      let _, name = captured.(i) in
      let var = lookup_var scope env name in
      let capture = Vm.VVI (Vm.Capture, dest_var, var, i) in
      iter (capture :: proc) (i + 1)
    in
    let captures = iter [] 0 in
    makefunc :: captures
  in
  {funcs = (func :: funcs); proc = proc}

let walk_decl global_scope entry_env globenv = function
  | Typed_ast.ValueBindings b ->
    let scope = new_scope global_scope in
    let env = new_env () in
    let scope, output = walk_value_bindings scope env globenv b in
    let var_list = gen_globals globenv scope in
    let gvar_list, _ = List.split var_list in
    let global_scope = List.fold_left (add_global globenv) global_scope gvar_list in
    let func =
      let id = new_func globenv ~prefix:"init" in
      let nvars = get_number_of_vars env in
      let proc =
        let iter proc (gnameid, nameid) =
          let gvar = Vm.Global gnameid in
          let var = Vm.Local nameid in
          let move = Vm.VV (Vm.Move, gvar, var) in
          move :: proc
        in
        let moves = List.fold_left iter [] var_list in
        let ret = Vm.S (Vm.Ret) in
        output.proc @ moves @ [ret]
      in
      Vm.{id; captured = [||]; nvars; proc}
    in
    let proc =
      let tmp_var = Vm.Local (new_var entry_env ~prefix:"var") in
      let id, name = func.id in
      let makefunc = Vm.D (Vm.MakeFunc, (id, name), tmp_var) in
      let call = Vm.V (Vm.Call, tmp_var) in
      [makefunc; call]
    in
    global_scope, {funcs = (func :: output.funcs); proc}

let reorder_captures (func : Vm.func) =
  let nvars = func.nvars in
  let proc = func.proc in
  let assign_cnt = Array.make nvars 0 in
  let inc_cnt = function
    | Vm.Local (id, _) -> assign_cnt.(id) <- assign_cnt.(id) + 1
    | _ -> ()
  in
  let count_assign = function
    (* TODO *)
    | Vm.VV (Vm.Move, dest, _) -> inc_cnt dest
    | _ -> ()
  in
  let () = List.iter count_assign proc in
  let pending_ssas = Array.make nvars [] in
  let ordered_proc = ref [] in
  let add_ssa ssa = ordered_proc := ssa :: !ordered_proc in
  let add_ssas ssas = ordered_proc := ssas @ !ordered_proc in
  let add_pending var ssa = 
    (match var with
    | Vm.Local (id, _) ->
      if assign_cnt.(id) > 0 then pending_ssas.(id) <- ssa :: pending_ssas.(id)
      else add_ssa ssa
    | _ -> add_ssa ssa)
  in
  let dec_cnt = function
    | Vm.Local (id, _) ->
      let cnt = assign_cnt.(id) - 1 in
      assign_cnt.(id) <- cnt;
      if cnt = 0 then add_ssas pending_ssas.(id)
    | _ -> ()
  in
  let reorder_proc ssa =
    match ssa with
    | Vm.VVI (Vm.Capture, _, src, _) -> add_pending src ssa
    | Vm.VV (Vm.Move, dest, _) -> add_ssa ssa; dec_cnt dest
    | _ -> add_ssa ssa
  in
  let () = List.iter reorder_proc proc in
  let proc = List.rev (!ordered_proc) in
  {func with proc}

let walk_decl_list l =
  let global_scope = root_scope in
  let entry_env = new_env () in
  let globenv = new_globenv () in
  let iter (scope, acc_outputs) e =
    let scope, output = walk_decl scope entry_env globenv e in
    scope, (output :: acc_outputs)
  in
  let global_scope, outputs = List.fold_left iter (global_scope, []) l in
  let output = List.fold_right concat_outputs (List.rev outputs) empty_output in
  let globs = Array.of_list (get_vars global_scope) in
  let funcs = Array.of_list output.funcs in
  let () =
    let cmp (a:Vm.func) (b:Vm.func) = compare a.id b.id in
    Array.sort cmp funcs
  in
  let entry =
    let id = (-1, "") in
    let nvars = get_number_of_vars entry_env in
    let proc =
      let ret = Vm.S (Vm.Ret) in
      output.proc @ [ret]
    in
    Vm.{id; captured=[||]; nvars; proc}
  in
  let () = Array.iter Vm.link_labels funcs in
  let funcs = Array.map reorder_captures funcs in
  Vm.{exts=Vm.bltin_funcs; globs; funcs; entry}
