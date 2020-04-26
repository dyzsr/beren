open Types
open Utils

(* Walk the AST to perform type inference and checking *)
exception Cyclic_types
exception Same_constructors_name of string

(* get constructor names from type definitions
  @consmap : constructors map
  @typ : toplevel type definition *)
let get_constructors (consmap : (variant * datatype) Name_map.t) typ =
  let fold dt consmap ((_, name, x) as variant) =
    let update = function
    | None -> Some (variant, dt)
    | Some _ -> raise (Same_constructors_name name)
    in
    Name_map.update name update consmap
  in
  match typ with
  | Alias (_, Generics (_, _, Variants (id, l))) as dt ->
    List.fold_left (fold dt) consmap l
  | Alias (_, Variants (id, l)) as dt ->
    List.fold_left (fold dt) consmap l
  | _ -> consmap

let merge_string_maps map1 map2 =
  let merge key a b =
    match a, b with
    | None, None -> None
    | Some a, None -> Some a
    | _, Some b -> Some b
  in
  Name_map.merge merge map1 map2

let rec walk_type_bindings symtab (r, l) = 
  let make_pending ((_, name, _) : Ast.type_binding) =
    (name, Pending name) in
  let make_alias (name, typ) =
    (name, Alias (name, typ)) in
  let from_binding symtab ((_, name, _) as binding : Ast.type_binding) =
    (* let () = print_endline ("from binding " ^ name) in *)
    (name, walk_type_binding symtab binding) in
  let add_binding symtab (name, typ) =
    add_type name typ symtab in
  let update_binding symtab (name, typ) =
    (name, update_pending symtab typ) in
  let resolve_binding symtab = function
    | (_, Alias (_, typ)) -> resolve_pending symtab typ
    | (_, typ) -> resolve_pending symtab typ in
  let filter_binding (_, typ) =
    filter_pending typ in
  (* let print (_, typ) =
    print_endline (string_of_toplevel_datatype typ) in *)
  let symtab, bindings =
    (* recursive definitions *)
    if r then
      let pendings = List.map make_pending l in
      let symtab = List.fold_left add_binding symtab pendings in
      let pendings = List.rev_map (from_binding symtab) l in
      let rec iter symtab completes pendings = function
      | 0 -> (symtab, completes, pendings)
      | n ->
        (* let () = print_endline ("iter pending " ^ string_of_int n) in *)
        if List.length pendings = 0 then (symtab, completes, pendings) else
        let bindings = List.map (update_binding symtab) pendings in
        let (pendings, bindings) = List.partition filter_binding bindings in
        let symtab = List.fold_left add_binding symtab pendings in
        let bindings = List.map make_alias bindings in
        let symtab = List.fold_left add_binding symtab bindings in
        iter symtab (completes @ bindings) pendings (n-1)
      in
      let n = List.length pendings in
      let (symtab, completes, pendings) = iter symtab [] pendings n in
      if List.length pendings > 0 then
        (* cannot resolve all pending types *)
        raise Cyclic_types
      else
        let () = List.iter (resolve_binding symtab) completes in
        symtab, completes
    (* non-recursive definitions *)
    else
      let rec iter symtab acc = function
      | [] -> symtab, acc
      | binding :: rest ->
        let binding = make_alias (from_binding symtab binding) in
        let symtab = add_binding symtab binding in
        iter symtab (binding :: acc) rest
      in
      let symtab, bindings = iter symtab [] l in
      symtab, (List.rev bindings)
  in
  (* check types *)
  let typs = List.map (fun (_, t) -> t) bindings in
  let () = List.iter check_datatype typs in
  (* insert constructors *)
  let consmap = List.fold_left get_constructors Name_map.empty typs in
  let constructors = merge_string_maps symtab.constructors consmap in
  { symtab with constructors=constructors }

and walk_type_binding symtab ((params_opt, name, construct): Ast.type_binding) =
  (* let () = print_endline ("from type binding: " ^ name) in *)
  let tvtab = empty_tvtab in
  match params_opt with
  | None -> 
    walk_type_construct symtab tvtab construct
  | Some tyvars ->
    let tyvars = List.map (fun x -> new_tyvar x) tyvars in
    let update_tvtab tvtab (name, id) =
      bind_tyvar name id tvtab in
    let tvtab = List.fold_left update_tvtab tvtab tyvars in
    let typ = walk_type_construct symtab tvtab construct in
    Generics (new_generic_id (), tyvars, typ)

and walk_type_construct symtab tvtab = function
  | Ast.TypeExpr t -> walk_type_expr symtab tvtab t
  | Ast.VariantsType t -> walk_variants_type symtab tvtab t
  | Ast.RecordType t -> walk_record_type symtab tvtab t
  | Ast.InterfaceType t -> walk_interface_type symtab tvtab t

and walk_type_op symtab tvtab is_decl =
  (* let () = print_endline ("from type expr") in *)
  function
  | Ast.SingleType expr ->
    (match expr with
    | Ast.TypeName name ->
      tvtab, (lookup_type name symtab : datatype)
    | Ast.TypeSymbol name ->
      if is_decl then
        tvtab, Tyvar (name, lookup_tyvar name tvtab)
      else
        match lookup_tyvar_opt name tvtab with
        | None ->
          let id = new_tyvar_id () in
          let tvtab = bind_tyvar name id tvtab in
          tvtab, Tyvar (name, id)
        | Some id ->
          tvtab, Tyvar (name, id)
    )
  | Ast.TupleType l ->
    let rec iter tvtab acc = function
    | [] -> tvtab, acc
    | x :: l ->
      let tvtab, typ = walk_type_op symtab tvtab is_decl x in
      iter tvtab (typ :: acc) l
    in
    let tvtab, typs = iter tvtab [] l in
    tvtab, Tuple (List.rev typs)
  | Ast.FunctionType (arg, body) ->
    let tvtab, arg = walk_type_op symtab tvtab is_decl arg in
    let tvtab, body = walk_type_op symtab tvtab is_decl body in
    tvtab, Function (arg, body)
  | Ast.SpecificType (args, name) ->
    let aux (tvtab, acc_args) arg =
      let tvtab, arg = walk_type_op symtab tvtab is_decl arg in
      tvtab, (arg :: acc_args)
    in
    let tvtab, args = List.fold_left aux (tvtab, []) args in
    tvtab, Specific (List.rev args, lookup_type name symtab)

and walk_type_expr symtab tvtab t =
  let _, typ = walk_type_op symtab tvtab true t in typ

and walk_type_annotation symtab tvtab t =
  walk_type_op symtab tvtab false t

and walk_variants_type symtab tvtab (l : Ast.variants_type) =
  (* let () = print_endline ("from variants type") in *)
  let from_variant i = function
  | (name, None) -> (i, name, None)
  | (name, Some expr) -> (i, name, Some (ref (walk_type_expr symtab tvtab expr)))
  in
  Variants (new_variants_id (), (List.mapi from_variant l))

and walk_record_type symtab tvtab (l : Ast.record_type) =
  (* let () = print_endline ("from record type") in *)
  let from_field i (name, expr) = (i, name, ref (walk_type_expr symtab tvtab expr)) in
  Record (new_record_id (), (List.mapi from_field l))

and walk_interface_type symtab tvtab (l : Ast.interface_type) =
  (* let () = print_endline ("from interface type") in *)
  let from_method (name, expr) = (name, ref (walk_type_expr symtab tvtab expr)) in
  Interface (List.map from_method l)


(* Match destination type with source type in a value binding *)

exception Type_mismatch
exception Incompatible_types of string * string

let raise_incompatible_types d s =
  let d = get_type_string d in
  let s = get_type_string s in
  raise (Incompatible_types (d, s))

let rec agree_on_type tvtab dest src =
  let () =
    print_value2 "~dest" dest (apply_real_type tvtab dest);
    print_value2 "~src" src (apply_real_type tvtab src)
  in
  match dest, src with
  | Generics _, _ -> failwith "agree_on_type: generics"
  | _, Generics _ -> failwith "agree_on_type: generics"

  | Alias (name, d), s -> agree_on_type tvtab d s
  | d, Alias (name, s) -> agree_on_type tvtab d s

  | Unit, Unit | Bool, Bool | Int, Int | Char, Char | String, String
  | Ref, Ref | Array, Array -> tvtab

  (* union-find *)
  | Tyvar (_, id1), Tyvar (_, id2) ->
    let tvtab, t1 = find_id id1 tvtab in
    let tvtab, t2 = find_id id2 tvtab in
    let tvtab = (match t1, t2 with
    | Tyvar (_, root1), Tyvar (_, root2) when root1 = root2 ->
      tvtab
    | t1, Tyvar (_, root2) ->
      add_id root2 t1 tvtab
    | Tyvar (_, root1), t2 ->
      add_id root1 t2 tvtab
    | t1, t2 ->
      agree_on_type tvtab t1 t2
    ) in
    let () = print_value2 "++merged++" t1 t2 in
    let () = print_value2 "**merged**" (apply_real_type tvtab t1) (apply_real_type tvtab t2) in
    tvtab
  | d, Tyvar (_, id) ->
    (* let () = print_endline "agree_on_type: dest type_var" in *)
    let tvtab, t = find_id id tvtab in
    (match t with
    | Tyvar (_, root) -> add_id root d tvtab
    | _ -> agree_on_type tvtab d t
    )
  | Tyvar (_, id), s ->
    (* let () = print_endline "agree_on_type: type_var src" in *)
    let tvtab, t = find_id id tvtab in
    (match t with
    | Tyvar (_, root) -> add_id root s tvtab
    | _ -> agree_on_type tvtab t s
    )

  | (Tuple dest as d), (Tuple src as s) ->
    (try List.fold_left2 agree_on_type tvtab dest src
    with Incompatible_types _ -> raise_incompatible_types d s
    )
  | Variants (did, d), Variants (sid, s) ->
    if did = sid then tvtab else raise Type_mismatch
  | Record (did, d), Record (sid, s) ->
    if did = sid then tvtab else raise Type_mismatch
  | Interface d, s ->
    failwith "agree_on_interface"
  | Function (darg, dbody), Function (sarg, sbody) ->
    let tvtab = agree_on_type tvtab darg sarg in
    agree_on_type tvtab dbody sbody
  (* | Generics (did, dvars, dbody), Generics (sid, svars, sbody) ->
    agree_on_generics tvtab (did, dvars, dbody) (sid, svars, sbody) *)
  | (Specific _ as dest), (Specific _ as src) ->
    let dest = apply_generics tvtab dest in
    let src = apply_generics tvtab src in
    agree_on_specifics tvtab dest src
  | (Specific _ as dest), src ->
    let dest = apply_generics tvtab dest in
    agree_on_specifics tvtab dest src
  | dest, (Specific _ as src) ->
    let src = apply_generics tvtab src in
    agree_on_specifics tvtab dest src
  (* | Specific (darg, Alias (_, Generics (did, dvars, dbody))),
    Specific (sarg, Alias (_, Generics (sid, svars, sbody))) ->
    let tvtab = List.fold_left2 agree_on_type tvtab darg sarg in
    agree_on_generics tvtab (did, dvars, dbody) (sid, svars, sbody) *)
  | d, s ->
    try raise_incompatible_types d s
    with Failure msg -> failwith ("agree_on_type: " ^ msg)

and agree_on_specifics tvtab dest src =
  match dest, src with
  | Specific (dargs, Alias (dname, Generics (did, _, _))),
    Specific (sargs, Alias (sname, Generics (sid, _, _))) ->
    if did <> sid
    then raise (Incompatible_types (dname, sname))
    else List.fold_left2 agree_on_type tvtab dargs sargs
  | (Specific _ as d), s | d, (Specific _ as s) ->
    (try
      let d = get_type_string d in
      let s = get_type_string s in
      raise (Incompatible_types (d, s)) 
    with
    | Failure msg -> failwith ("agree_on_type: " ^ msg)
    )
  | d, s ->
    agree_on_type tvtab d s
(* 
and agree_on_generics tvtab t1 t2 =
  let did = extract_generics_id t1 in
  let sid = extract_generics_id t2 in
  if did = sid then tvtab else raise Type_mismatch *)

(* Extract types from value bindings *)
exception Arity_mismatch of string

let fold_symtab symtab (k, v) =
  let () = print_value k v in
  add_value k v symtab

let make_tyvars (tyvars : tyvar list) =
  let idmapping = List.map (fun (name, id) -> id, Tyvar (new_tyvar name)) tyvars in
  let idmap = Id_map.of_seq (List.to_seq idmapping) in
  let tvtab = {empty_tvtab with idmap} in
  let _, tyvars = List.split idmapping in
  match tyvars with
  | [] -> failwith "walk_capident: make_tyvars"
  | l -> l, tvtab

let rec substitute_tyvars tvtab = function
  | Unit | Bool | Int | Char | String | Ref | Array as t -> t
  | Tyvar (_, id) ->
    (match lookup_id id tvtab with
     | Some t -> t
     | None -> failwith "substitute_tyvars: id is not found")
  | Tuple l -> Tuple (List.map (substitute_tyvars tvtab) l)
  | Variants _ | Record _ | Interface _ | Generics _ as t -> t
  | Function (arg, body) -> Function (substitute_tyvars tvtab arg, substitute_tyvars tvtab body)
  | Specific (args, body) ->
    let args = List.map (substitute_tyvars tvtab) args in
    Specific (args, body)
  | Alias (name, typ) -> Alias (name, substitute_tyvars tvtab typ)
  | _ -> failwith "substitute_tyvars"

let combine3 a b c =
  let rec iter acc = function
  | [], [], [] -> acc
  | (a::al), (b::bl), (c::cl) -> iter ((a,b,c) :: acc) (al, bl, cl)
  | _, _, _ -> failwith "combine3"
  in
  List.rev (iter [] (a, b, c))

let rec walk_value_bindings symtab tvtab (r, l) =
  if r then (* recursive *)
    (* add bindings to the symbol table before getting real types *)
    let init_bindings 
      (symtab, tvtab, pattern_typs, acc_bindings, acc_asts, acc_names)
      (pattern, _) =
      let bindings, tvtab, typ, ast = walk_pattern symtab tvtab pattern in
      let names, _ = List.split bindings in
      let symtab = List.fold_left fold_symtab symtab bindings in
      symtab, tvtab, (typ::pattern_typs), (bindings@acc_bindings), 
      (ast::acc_asts), (names::acc_names)
    in
    let symtab, tvtab, pattern_typs, bindings, pattern_asts, acc_names =
      List.fold_left init_bindings (symtab, tvtab, [], [], [], []) l in
    let pattern_typs = List.rev pattern_typs in
    let pattern_asts = List.rev pattern_asts in
    let acc_names = List.rev acc_names in
    (* fill the bindings with real types *)
    let fill_bindings (tvtab, acc_asts) (pattern_typ, (_, expr)) =
      let tvtab, expr_typ, expr_ast = walk_expr symtab tvtab expr in
      let tvtab = agree_on_type tvtab pattern_typ expr_typ in
      let () =
        print_value "[pattern]" (apply_real_type tvtab pattern_typ);
        print_value "[expr]" (apply_real_type tvtab expr_typ)
      in
      tvtab, (expr_ast :: acc_asts)
    in
    let tvtab, expr_asts =
      List.fold_left fill_bindings (tvtab, []) (List.combine pattern_typs l) in
    let expr_asts = List.rev expr_asts in
    let binding_asts = combine3 acc_names pattern_asts expr_asts in
    let ast =
      Typed_ast.update_typed_ast tvtab (Typed_ast.ValueBindings (r, binding_asts)) in
    let bindings = List.map (fun (k, v) -> k, apply_real_type tvtab v) bindings in
    let symtab = List.fold_left fold_symtab symtab bindings in
    symtab, tvtab, ast
  else (* non-recursive *)
    let walk_value_binding (pattern, expr) =
      let bindings, tvtab, pattern_typ, pattern_ast = walk_pattern symtab tvtab pattern in
      let names, _ = List.split bindings in
      let tvtab, expr_typ, expr_ast = walk_expr symtab tvtab expr in
      let tvtab = agree_on_type tvtab pattern_typ expr_typ in
      let () =
        print_value "[pattern]" (apply_real_type tvtab pattern_typ);
        print_value "[expr]" (apply_real_type tvtab expr_typ)
      in
      let bindings = List.map (fun (k, v) -> k, apply_real_type tvtab v) bindings in
      bindings, (names, pattern_ast, expr_ast)
    in
    let bindings_list, binding_asts = List.split (List.map walk_value_binding l) in
    let bindings = List.fold_left (@) [] bindings_list in
    let ast =
      Typed_ast.update_typed_ast tvtab (Typed_ast.ValueBindings (r, binding_asts)) in
    List.fold_left fold_symtab symtab bindings, tvtab, ast

and walk_expr symtab tvtab = function
  | Ast.Unit -> tvtab, Unit, Typed_ast.Unit Unit
  | Ast.Bool b -> tvtab, Bool, Typed_ast.Bool (b, Bool)
  | Ast.Int i -> tvtab, Int, Typed_ast.Int (i, Int)
  | Ast.Char c -> tvtab, Char, Typed_ast.Char (c, Char)
  | Ast.String s -> tvtab, String, Typed_ast.String (s, String)
  | Ast.Call (Ast.CapIdent name, e) -> walk_construct symtab tvtab (name, e)
  | Ast.CapIdent name -> walk_capident symtab tvtab name
  | Ast.Variable v -> walk_variable symtab tvtab v
  | Ast.Assign (v, e) -> walk_assign symtab tvtab (v, e)
  | Ast.Tuple l ->
    let rec iter tvtab acc acc_asts = function
    | [] -> tvtab, acc, acc_asts
    | e :: l ->
      let tvtab, typ, ast = walk_expr symtab tvtab e in
      iter tvtab (typ :: acc) (ast :: acc_asts) l
    in
    let tvtab, typs, asts = iter tvtab [] [] l in
    let asts = List.rev asts in
    let typ = Tuple (List.rev typs) in
    tvtab, typ, Typed_ast.Tuple (asts, typ)
  | Ast.List l -> walk_list symtab tvtab l
  | Ast.Array l -> walk_array symtab tvtab l
  | Ast.Record l -> walk_record symtab tvtab l
  | Ast.Call (c, e) -> walk_call symtab tvtab (c, e)
  | Ast.Unary (op, e) -> walk_unary symtab tvtab (op, e)
  | Ast.Binary (op, a, b) -> walk_binary symtab tvtab (op, a, b)
  | Ast.Local (b, e) -> walk_local symtab tvtab (b, e)
  | Ast.IfExpr e -> walk_if_expr symtab tvtab e
  | Ast.MatchExpr e -> walk_match_expr symtab tvtab e
  | Ast.LambdaExpr (arg, body) -> walk_lambda_expr symtab tvtab (arg, body)
  | Ast.ExprList l ->
    let aux (tvtab, acc_typ, acc_asts) e =
      let tvtab, typ, ast = walk_expr symtab tvtab e in
      tvtab, typ, (ast :: acc_asts)
    in
    let tvtab, typ, asts = List.fold_left aux (tvtab, Unit, []) l in
    tvtab, typ, Typed_ast.ExprList (List.rev asts, typ)
  | Ast.ExprWithType (e, t) ->
    let tvtab, e_typ, e_ast = walk_expr symtab tvtab e in
    let tvab, t_typ = walk_type_annotation symtab tvtab t in
    let tvtab = agree_on_type tvtab t_typ e_typ in
    tvtab, t_typ, Typed_ast.ExprWithType (e_ast, t_typ)

and walk_capident symtab tvtab name =
  let variant, typ = lookup_constructor name symtab in
  match variant with
  | _, name, Some _ -> raise (Arity_mismatch name)
  | _, name, None ->
  let typ =
    match typ with
    | Alias (_, Generics (_, var_names, _)) ->
      let tyvars, varmap = make_tyvars var_names in
      Specific (tyvars, typ)
    | t -> t
  in
  let () = print_value name typ in
  tvtab, typ, Typed_ast.Variant (variant, typ)

and walk_construct symtab tvtab (name, e) =
  let variant, typ = lookup_constructor name symtab in
  match variant with
  | _, name, None -> raise (Arity_mismatch name)
  | _, name, Some {contents=param} ->
    let param_typ, ret_typ = match typ with
      | Alias (_, Generics (_, var_names, _)) ->
        let tyvars, varmap = make_tyvars var_names in
        let param = substitute_tyvars varmap param in
        param, Specific (tyvars, typ)
      | t -> param, t
    in
    let tvtab, arg_typ, arg = walk_expr symtab tvtab e in
    let tvtab = agree_on_type tvtab param_typ arg_typ in
    let () = print_value name typ in
    tvtab, typ, Typed_ast.Construct (variant, arg, ret_typ)

and walk_variable symtab tvtab = function
  | None, name ->
    let var = lookup_value name symtab in
    let () = print_value2 ("$" ^ name) var (apply_real_type tvtab var) in
    tvtab, var, Typed_ast.Variable (None, name, var)
  | _ ->
    failwith "walk_variable: not implemented"

and walk_assign symtab tvtab (v, e) =
  let tvtab, assignee, assignee_ast = walk_variable symtab tvtab v in
  let tvtab, assigner, assigner_ast = walk_expr symtab tvtab e in
  (match assignee with
   | Specific ([tyvar], typ) when typ = ref_type ->
     let tvtab = agree_on_type tvtab tyvar assigner in
     tvtab, Unit, Typed_ast.Assign (assignee_ast, assigner_ast, Unit)
   | _ -> failwith "walk_assign")

and walk_list symtab tvtab l =
  let tvtab, typ, asts = walk_item_list symtab tvtab l in
  let typ = Specific ([typ], list_type) in
  tvtab, typ, Typed_ast.List (asts, typ)

and walk_array symtab tvtab l =
  let tvtab, typ, asts = walk_item_list symtab tvtab l in
  let typ = Specific ([typ], array_type) in
  tvtab, typ, Typed_ast.Array (asts, typ)

and walk_item_list symtab tvtab l =
  let acc_typ = Tyvar (new_tyvar "a") in
  let aux (tvtab, acc_asts) e =
    let tvtab, typ, ast = walk_expr symtab tvtab e in
    let tvtab = agree_on_type tvtab acc_typ typ in
    tvtab, (ast :: acc_asts)
  in
  let tvtab, acc_asts = List.fold_left aux (tvtab, []) l in
  let acc_asts = List.rev acc_asts in
  tvtab, acc_typ, acc_asts

and walk_record symtab tvtab l =
  failwith "walk_record"

and walk_call symtab tvtab (c, e) =
  let tvtab, caller, caller_ast = walk_expr symtab tvtab c in
  let tvtab, callee, callee_ast = walk_expr symtab tvtab e in
  let arg = Tyvar (new_tyvar "a") in
  let body = Tyvar (new_tyvar "a") in
  let func = Function (arg, body) in
  let tvtab = agree_on_type tvtab func caller in
  let tvtab = agree_on_type tvtab arg callee in
  let () = print_value2 "[caller]" func (apply_real_type tvtab func) in
  let () = print_value2 "[callee]" arg (apply_real_type tvtab arg) in
  tvtab, body, Typed_ast.Call (caller_ast, callee_ast, body)

and walk_unary symtab tvtab (op, e) =
  let tvtab, operand, operand_ast = walk_expr symtab tvtab e in
  match op with
  | Ast.Positive | Ast.Negative ->
    let tvtab = agree_on_type tvtab Int operand in
    tvtab, Int, Typed_ast.Unary (op, operand_ast, Int)
  | Ast.Deref -> 
    let typ = Tyvar (new_tyvar "a") in
    let ref_typ = Specific ([typ], ref_type) in
    let tvtab = agree_on_type tvtab ref_typ operand in
    tvtab, typ, Typed_ast.Unary (op, operand_ast, typ)

and walk_binary symtab tvtab (op, a, b) =
  let tvtab, a, a_ast = walk_expr symtab tvtab a in
  let tvtab, b, b_ast = walk_expr symtab tvtab b in
  match op with
  | Ast.Plus | Ast.Minus | Ast.Times | Ast.Div | Ast.Mod ->
    let tvtab = agree_on_type tvtab Int a in
    let tvtab = agree_on_type tvtab Int b in
    tvtab, Int, Typed_ast.Binary (op, a_ast, b_ast, Int)
  | Ast.Lt | Ast.Lte | Ast.Gt | Ast.Gte | Ast.Eq | Ast.Neq ->
    let tvtab = agree_on_type tvtab a b in
    tvtab, Bool, Typed_ast.Binary (op, a_ast, b_ast, Bool)
  | Ast.And | Ast.Or ->
    let tvtab = agree_on_type tvtab Bool a in
    let tvtab = agree_on_type tvtab Bool b in
    tvtab, Bool, Typed_ast.Binary (op, a_ast, b_ast, Bool)
  | Ast.Cons ->
    let list_typ = Specific ([a], lookup_type "list" symtab) in
    let tvtab = agree_on_type tvtab list_typ b in
    tvtab, list_typ, Typed_ast.Binary (op, a_ast, b_ast, list_typ)
  | Ast.Append ->
    let list_typ = Specific ([Tyvar (new_tyvar "a")], lookup_type "list" symtab) in
    let tvtab = agree_on_type tvtab list_typ a in
    let tvtab = agree_on_type tvtab list_typ b in
    tvtab, list_typ, Typed_ast.Binary (op, a_ast, b_ast, list_typ)
  | Ast.Concat ->
    let tvtab = agree_on_type tvtab String a in
    let tvtab = agree_on_type tvtab String b in
    tvtab, String, Typed_ast.Binary (op, a_ast, b_ast, String)

and walk_local symtab tvtab (bindings, e) =
  let symtab, tvtab, binding_ast = walk_value_bindings symtab tvtab bindings in
  let tvtab, typ, expr_ast = walk_expr symtab tvtab e in
  tvtab, typ, Typed_ast.Local (binding_ast, expr_ast, typ)

and walk_if_expr symtab tvtab (cond, then_e, else_opt) =
  let tvtab, cond, cond_ast = walk_expr symtab tvtab cond in
  let tvtab = agree_on_type tvtab Bool cond in
  let tvtab, then_t, then_ast = walk_expr symtab tvtab then_e in
  match else_opt with
  | None ->
    let tvtab = agree_on_type tvtab Unit then_t in
    tvtab, Unit, Typed_ast.IfExpr (cond_ast, then_ast, None, Unit)
  | Some else_e ->
    let tvtab, else_t, else_ast = walk_expr symtab tvtab else_e in
    let tvtab = agree_on_type tvtab then_t else_t in
    tvtab, then_t, Typed_ast.IfExpr (cond_ast, then_ast, Some else_ast, then_t)

and walk_match_expr symtab tvtab (e, l) =
  let tvtab, acc_pattern, data_ast = walk_expr symtab tvtab e in
  let acc_body = Tyvar (new_tyvar "a") in
  let () = print_value "[body]" acc_body in
  let aux (tvtab, branch_asts) (pattern, body) =
    let bindings, tvtab, pattern, pattern_ast = walk_pattern symtab tvtab pattern in
    let varnames = List.map fst bindings in
    let tvtab = agree_on_type tvtab acc_pattern pattern in
    let () = print_value2 "[branch-pattern]" pattern (apply_real_type tvtab pattern) in
    let bindings = List.map (fun (k, v) -> k, apply_real_type tvtab v) bindings in
    let symtab = List.fold_left fold_symtab symtab bindings in
    let tvtab, body, body_ast = walk_expr symtab tvtab body in
    let tvtab = agree_on_type tvtab acc_body body in
    let () = print_value2 "[branch-body]" body (apply_real_type tvtab body) in
    tvtab, ((varnames, pattern_ast, body_ast, body) :: branch_asts)
  in
  let tvtab, branch_asts = List.fold_left aux (tvtab, []) l in
  let branch_asts = List.rev branch_asts in
  let typ = acc_body in
  let () = print_value2 "[match]" typ (apply_real_type tvtab typ) in
  tvtab, typ, Typed_ast.MatchExpr (data_ast, branch_asts, typ)

and walk_lambda_expr symtab tvtab (arg, body) =
  let bindings, tvtab, arg, arg_ast = walk_pattern symtab tvtab arg in
  let varnames = List.map fst bindings in
  let bindings = List.map (fun (k, v) -> k, apply_real_type tvtab v) bindings in
  let symtab = List.fold_left fold_symtab symtab bindings in
  let tvtab, body, body_ast = walk_expr symtab tvtab body in
  let func = Function (arg, body) in
  let () = print_value2 "[fun]" func (apply_real_type tvtab func) in
  tvtab, func, Typed_ast.LambdaExpr (varnames, arg_ast, body_ast, func)

and walk_pattern symtab tvtab = function
  | Ast.BoolPattern b -> [], tvtab, Bool, Typed_ast.BoolPattern (b, Bool)
  | Ast.IntPattern i -> [], tvtab, Int, Typed_ast.IntPattern (i, Int)
  | Ast.CharPattern c -> [], tvtab, Char, Typed_ast.CharPattern (c, Char)
  | Ast.StringPattern s -> [], tvtab, String, Typed_ast.StringPattern (s, String)
  | Ast.UnitPattern -> [], tvtab, Unit, Typed_ast.UnitPattern Unit
  | Ast.TuplePattern l ->
    let rec aux (tvtab, bindings, acc_typs, acc_asts) p =
      let new_bindings, tvtab, typ, ast = walk_pattern symtab tvtab p in
      tvtab, (new_bindings @ bindings), (typ :: acc_typs), (ast :: acc_asts)
    in
    let tvtab, bindings, typs, asts = List.fold_left aux (tvtab, [], [], []) l in
    let bindings = List.rev bindings in
    let typ = Tuple (List.rev typs) in
    bindings, tvtab, typ, Typed_ast.TuplePattern (List.rev asts, typ)
  | Ast.ConsPattern (hd, tl) ->
    walk_variant_pattern symtab tvtab ("(::)", Some (Ast.TuplePattern [hd; tl]))
  | Ast.ListPattern l ->
    let bindings, tvtab, typ, asts = walk_pattern_item_list symtab tvtab l in
    let typ = Specific ([typ], list_type) in
    bindings, tvtab, typ, Typed_ast.ListPattern (asts, typ)
  | Ast.ArrayPattern l ->
    let bindings, tvtab, typ, asts = walk_pattern_item_list symtab tvtab l in
    let typ = Specific ([typ], array_type) in
    bindings, tvtab, typ, Typed_ast.ArrayPattern (asts, typ)
  | Ast.RecordPattern l ->
    walk_record_pattern symtab tvtab l
  | Ast.RefPattern p ->
    let bindings, tvtab, typ, ast = walk_pattern symtab tvtab p in
    let typ = Specific ([typ], ref_type) in
    bindings, tvtab, typ, Typed_ast.RefPattern (ast, typ)
  | Ast.VariablePattern name ->
    let typ = Tyvar (new_tyvar "a") in
    let () = print_value ("<" ^ name ^ ">") typ in
    let binding = (name, typ) in
    [binding], tvtab, typ, Typed_ast.VariablePattern (name, typ)
  | Ast.Wildcard ->
    let typ = Tyvar (new_tyvar "a") in
    let () = print_value "_" typ in
    [], tvtab, typ, Typed_ast.Wildcard typ
  | Ast.VariantPattern (name, p) ->
    walk_variant_pattern symtab tvtab (name, p)
  | Ast.PatternList l ->
    walk_pattern_list symtab tvtab l
  | Ast.PatternWithType (p, t) ->
    let bindings, tvtab, p_typ, p_ast = walk_pattern symtab tvtab p in
    let tvtab, t_typ = walk_type_annotation symtab tvtab t in
    let tvtab = agree_on_type tvtab t_typ p_typ in
    bindings, tvtab, t_typ, Typed_ast.PatternWithType (p_ast, t_typ)

and walk_pattern_item_list symtab tvtab l =
  let acc_typ = Tyvar (new_tyvar "a") in
  let aux (bindings, tvtab, acc_asts) p =
    let new_bindings, tvtab, typ, ast = walk_pattern symtab tvtab p in
    let tvtab = agree_on_type tvtab acc_typ typ in
    (new_bindings @ bindings), tvtab, (ast :: acc_asts)
  in
  let bindings, tvtab, acc_asts = List.fold_left aux ([], tvtab, []) l in
  (List.rev bindings), tvtab, acc_typ, (List.rev acc_asts)

and walk_record_pattern symtab tvtab l = 
  failwith "walk_record_pattern"

and walk_variant_pattern symtab tvtab (name, pattern_opt) =
  let variant, typ = lookup_constructor name symtab in
  match pattern_opt with
  | None ->
    let typ = match typ with
      | Alias (_, Generics (_, tyvars, t)) ->
        let tyvars, varmap = make_tyvars tyvars in
        Specific (tyvars, typ)
      | _ -> typ
    in
    [], tvtab, typ, Typed_ast.VariantPattern (variant, None, typ)
  | Some p ->
    let bindings, tvtab, pattern_typ, pattern_ast = walk_pattern symtab tvtab p in
    let content_typ, typ = match variant with
      | (_, name, None) -> raise (Type_arity_mismatch name)
      | (_, name, Some {contents=content_typ}) ->
        match typ with
        | Alias (_, Generics (_, tyvars, t)) ->
          let tyvars, varmap = make_tyvars tyvars in
          substitute_tyvars varmap content_typ, Specific (tyvars, typ)
        | _ -> content_typ, typ
    in
    let tvtab = agree_on_type tvtab content_typ pattern_typ in
    bindings, tvtab, typ, Typed_ast.VariantPattern (variant, Some pattern_ast, typ)

and walk_pattern_list symtab tvtab l =
  let acc_typ = Tyvar (new_tyvar "a") in
  let rec aux (bindings_list, tvtab, pattern_asts) p =
    let bindings, tvtab, typ, ast = walk_pattern symtab tvtab p in
    let tvtab = agree_on_type tvtab acc_typ typ in
    (bindings :: bindings_list), tvtab, (ast :: pattern_asts)
  in
  let bindings_list, tvtab, pattern_asts = List.fold_left aux ([], tvtab, []) l in
  let compare (name1, _) (name2, _) =
    compare name1 name2
  in
  let bindings_list = List.map (List.sort compare) bindings_list in
  let rec iter tvtab acc = function
  | [] -> tvtab, acc
  | bindings :: l ->
    let bindings = List.combine acc bindings in
    let rec combine tvtab acc = function
    | [] -> tvtab, acc
    | ((acc_name, acc_typ), (binding_name, binding_typ)) :: l ->
      if acc_name <> binding_name then raise Type_mismatch else
      let tvtab = agree_on_type tvtab acc_typ binding_typ in
      combine tvtab ((acc_name, acc_typ) :: acc) l
    in
    let tvtab, bindings = combine tvtab [] bindings in
    iter tvtab bindings l
  in
  let tvtab, bindings = 
    (match bindings_list with
    | [] -> failwith "walk_pattern_list"
    | bindings :: l -> iter tvtab bindings l)
  in
  bindings, tvtab, acc_typ, Typed_ast.PatternList (pattern_asts, acc_typ)

let rec walk_method_bindings symtab (r, l) =
  failwith "walk_method_bindings"

let walk_decl symtab = function
  | Ast.TypeBindings b ->
    let symtab = walk_type_bindings symtab b in
    symtab, None
  | Ast.ValueBindings b ->
    let () = print_endline "# Walk value declaration" in
    let symtab, _, ast = walk_value_bindings symtab empty_tvtab b in
    symtab, Some (Typed_ast.ValueBindings ast)
  | Ast.MethodBindings b ->
    walk_method_bindings symtab b

let walk_decl_list l =
  let symtab = default_symtab in
  let fold (symtab, acc_asts) e =
    let symtab, ast_opt = walk_decl symtab e in
    match ast_opt with
    | None -> symtab, acc_asts
    | Some ast -> symtab, (ast :: acc_asts)
  in
  let symtab, asts = List.fold_left fold (symtab, []) l in
  let asts = List.rev asts in
  symtab, asts