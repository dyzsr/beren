type id = int

let init_id = ref 0

let current_id () =
  !init_id

let new_id () = 
  let id = !init_id in init_id := id + 1; id

type datatype =
  | Unit
  | Bool
  | Int
  | Char
  | String
  | Ref
  | Array
  | Param of param
  | Tuple of datatype list
  | Variants of id * variant list (* variants *)
  | Record of id * field list (* fields *)
  | Interface of signature list
  | Function of datatype * datatype
  | Generic of param list * datatype
  | Specific of datatype * datatype
  | Alias of string * datatype (* a complete type with an alias *)
  | Pending of string (* an incomplete type with only a name *)

and param = string
and variant = string * datatype ref option
and field = string (* name *) * datatype ref
and signature = string * datatype ref

let make_param name = Param name
let same_param (p1 : param) (p2 : param) = p1 = p2

let rec string_of_toplevel_datatype = function
  | Alias (name, Generic (params, typ)) ->
    let string_of_params = function
    | [x] -> "'" ^ x
    | l -> let params = List.map (fun x -> "'" ^ x) l in
      "(" ^ String.concat ", " params ^ ")"
    in
    let params_string = string_of_params params in
    (match typ with
     | Ref -> "type " ^ params_string ^ " ref"
     | Array -> "type " ^ params_string ^ " array"
     | _ -> "type " ^ params_string ^ " " ^ name ^ " = " ^ string_of_datatype typ)
  | Alias (name, typ) ->
      "type " ^ name ^ " = " ^ string_of_datatype typ
  | t -> "type _ = " ^ string_of_datatype t

and string_of_datatype = function
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Char -> "char"
  | String -> "string"
  | Ref -> failwith "string_of_datatype: ref"
  | Array -> failwith "string_of_datatype: array"
  | Param p -> "'" ^ p
  | Tuple l -> String.concat " * " (List.map string_of_datatype l)
  | Variants (_, l) ->
    let string_of_variant = function
    | (name, None) -> name
    | (name, Some typ) -> name ^ " of " ^ string_of_datatype (!typ)
    in
    String.concat " | " (List.map string_of_variant l)
  | Record (_, l) ->
    let string_of_field (name, typ) =
      name ^ " : " ^ string_of_datatype (!typ)
    in
    "{ " ^ String.concat "; " (List.map string_of_field l) ^ " }"
  | Interface l ->
    let string_of_signature (name, typ) =
      name ^ " : " ^ string_of_datatype (!typ)
    in
    "sig " ^ String.concat "; " (List.map string_of_signature l) ^ " end"
  | Function (arg, body) ->
    string_of_datatype arg ^ " -> " ^ string_of_datatype body
  | Generic (params, body) -> failwith "string_of_datatype: generic"
  | Specific (arg, body) ->
    string_of_datatype arg ^ " " ^ string_of_datatype body
  | Alias (name, _) -> name
  | Pending name -> "[" ^ name ^ "]"

(* Type tables *)
type typtab = {
  types : datatype Scope.t;
  symbols : datatype Scope.t;
  constructors : (variant * datatype) Scope.t;
  fields : (field * datatype) Scope.t;
  params : datatype Scope.t;
}

exception Unbound_type of string

let lookup_type name {types=types} = 
  match Scope.lookup name types with
  | None -> raise (Unbound_type name)
  | Some typ -> typ

let add_type name typ ({types=types} as typtab) =
  let types = Scope.add name typ types in
  {typtab with types=types}

exception Unbound_symbol of string

let lookup_symbol name {symbols=symbols} =
  match Scope.lookup name symbols with
  | None -> raise (Unbound_symbol name)
  | Some sym -> sym
  
let add_symbol name sym ({symbols=symbols} as typtab) =
  let symbols = Scope.add name sym symbols in
  {typtab with symbols=symbols}
  
exception Unbound_constructor of string

let lookup_constructor name {constructors=constructors} =
  match Scope.lookup name constructors with
  | None -> raise (Unbound_constructor name)
  | Some con -> con

let add_constructor name con ({constructors=constructors} as typtab) =
  let constructors = Scope.add name con constructors in
  {typtab with constructors=constructors}

exception Unbound_field of string

let lookup_field name {fields=fields} =
  match Scope.lookup name fields with
  | None -> raise (Unbound_field name)
  | Some field -> field

let add_field name field ({fields=fields} as typtab) =
  let fields = Scope.add name field fields in
  {typtab with fields=fields}

(* Type params *)
let lookup_param_opt name {params=params} =
  Scope.lookup name params

let param_is_bound name {params=params} =
  match Scope.lookup name params with
  | None -> false
  | Some _ -> true

let add_param name typ ({params=params} as typtab) =
  let params = Scope.add name typ params in
  {typtab with params=params}

let new_param {params=params} =
  let rec iter ch count =
    let name = if count = 0 then String.make 1 ch else String.make 1 ch ^ string_of_int count in
    match Scope.lookup name params with
    | None -> name
    | Some _ ->
      if ch = 'z' then iter 'a' (count+1)
      else iter (char_of_int (int_of_char ch + 1)) count
  in
  iter 'a' 0

(* Update pending types in tuple, function, and generics *)
let rec update_pending typtab = function
  | Unit -> Unit
  | Bool -> Bool
  | Int -> Int
  | Char -> Char
  | String -> String
  | Ref -> Ref
  | Array -> Array
  | Param _ as t -> t
  | Tuple l -> Tuple (List.map (update_pending typtab) l)
  | Variants _ as t -> t
  | Record _ as t -> t
  | Interface _ as t -> t
  | Function (arg, body) -> Function (update_pending typtab arg, update_pending typtab body)
  | Generic (params, body) -> Generic (params, update_pending typtab body)
  | Specific (arg, body) -> Specific (update_pending typtab arg, update_pending typtab body)
  | Alias _ as t -> t
  | Pending name -> lookup_type name typtab

(* Resolve pending types inside variants, record, and interface *)
let rec resolve_pending typtab = function
  | Unit -> ()
  | Bool -> ()
  | Int -> ()
  | Char -> ()
  | String -> ()
  | Ref -> ()
  | Array -> ()
  | Param _ -> ()
  | Tuple l -> List.iter (resolve_pending typtab) l
  | Variants (_, l) ->
    let (resolve_variant : variant -> unit) = function
    | (_, None) -> ()
    | (_, Some typ) -> typ := update_pending typtab (!typ)
    in
    List.iter resolve_variant l
  | Record (_, l) ->
    let resolve_field (_, typ : field) =
      typ := update_pending typtab (!typ)
    in
    List.iter resolve_field l
  | Interface l ->
    let resolve_signature (_, typ : signature) =
      typ := update_pending typtab (!typ)
    in
    List.iter resolve_signature l
  | Function (arg, body) -> resolve_pending typtab arg; resolve_pending typtab body
  | Generic (_, body) -> resolve_pending typtab body
  | Specific (arg, body) -> resolve_pending typtab arg; resolve_pending typtab body
  | Alias _ -> ()
  | Pending _ -> failwith "resolve_pending"

(* Filter types with pending inside *)
let rec filter_pending = function
  | Unit -> false
  | Bool -> false
  | Int -> false
  | Char -> false
  | String -> false
  | Ref -> false
  | Array -> false
  | Param _ -> false
  | Tuple l ->
    let rec iter = function
    | [] -> false
    | h :: t -> if filter_pending h then true else iter t
    in
    iter l
  | Variants _ -> false
  | Record _ -> false
  | Interface _ -> false
  | Function (arg, body) -> filter_pending arg || filter_pending body
  | Generic (_, body) -> filter_pending body
  | Specific (arg, body) -> filter_pending arg || filter_pending body
  | Alias (_, typ) -> false
  | Pending _ -> true


let rec extract_alias = function
  | Alias (_, typ) -> typ
  | t -> t

exception Type_arity_mismatch of string

let rec check_datatype = function
  | Unit -> ()
  | Bool -> ()
  | Int -> ()
  | Char -> ()
  | String -> ()
  | Ref -> failwith "check_datatype: ref"
  | Array -> failwith "check_datatype: array"
  | Param _ -> ()
  | Tuple l -> List.iter check_datatype l
  | Variants (_, l) -> let aux = function
    | (_, None) -> ()
    | (_, Some t) -> check_datatype (!t)
    in
    List.iter aux l
  | Record (_, l) -> List.iter (fun (_, t) -> check_datatype (!t)) l
  | Interface l -> List.iter (fun (_, t) -> check_datatype (!t)) l
  | Function (arg, body) -> check_datatype arg; check_datatype body
  | Generic (_, Ref) -> ()
  | Generic (_, Array) -> ()
  | Generic (_, body) -> check_datatype body
  | Specific (arg, body) -> check_datatype arg;
    (match body with
      | Alias (name, Generic _) -> ()
      | Alias (name, _) -> raise (Type_arity_mismatch name)
      | _ -> failwith "check_datatype: specific")
  | Alias (_, Alias (name, Generic _)) -> raise (Type_arity_mismatch name)
  | Alias _ -> ()
  | Pending _ -> failwith "check_datatype: pending"

(* Default types for access *)

(* define list *)
let ref_to_cons = ref (Tuple [Param "a"; Specific (Param "a", Pending "list")])

let list_nil = ("[]", None)

let list_cons = ("(::)", Some ref_to_cons)

let list_type =
  let variants = Variants (new_id (), [list_nil; list_cons]) in
  let alias = Alias ("list", Generic(["a"], variants)) in
  ref_to_cons := Tuple [Param "a"; Specific (Param "a", alias)];
  alias

(* define ref *)
let ref_type = Alias ("ref", Generic (["a"], Ref))

(* define array *)
let array_type = Alias ("array", Generic (["a"], Array))

(* default type table *)
let default_typtab =
  (* save default types *)
  let default_type_list =
    [("unit", Alias ("unit", Unit));
     ("bool", Alias ("bool", Bool));
     ("int", Alias ("int", Int));
     ("char", Alias ("char", Char));
     ("string", Alias ("string", String));
     ("list", list_type);
     ("ref", ref_type);
     ("array", array_type)]
  in
  let default_types = Scope.make default_type_list in
  let print (_, typ) = print_endline (string_of_toplevel_datatype typ) in
  let () = List.iter print default_type_list in
  (* save default symbols *)
  let default_symbol_list =
    [("ref", Function (Param "a", Specific (Param "a", ref_type)))] in
  let default_symbols = Scope.make default_symbol_list in
  (* save default constructors *)
  let default_constructor_list =
    [("[]", (list_nil, list_type));
     ("(::)", (list_cons, list_type))]
  in
  let default_constructors = Scope.make default_constructor_list in
  (* create the default type table *)
  { types = default_types;
    symbols = default_symbols;
    constructors = default_constructors;
    fields = Scope.empty ();
    params = Scope.empty (); }


(* Walk the AST to perform type inference and checking *)

exception Cyclic_types

let rec walk_type_bindings typtab (r, l) = 
  let make_pending ((_, name, _) : Ast.type_binding) =
    (name, Pending name) in
  let make_alias (name, typ) =
    (name, Alias (name, typ)) in
  let from_binding typtab ((_, name, _) as binding : Ast.type_binding) =
    (* let () = print_endline ("from binding " ^ name) in *)
    (name, datatype_of_type_binding typtab binding) in
  let add_binding typtab (name, typ) =
    add_type name typ typtab in
  let update_binding typtab (name, typ) =
    (name, update_pending typtab typ) in
  let resolve_binding typtab = function
    | (_, Alias (_, typ)) -> resolve_pending typtab typ
    | (_, typ) -> resolve_pending typtab typ in
  let filter_binding (_, typ) =
    filter_pending typ in
  let print (_, typ) =
    print_endline (string_of_toplevel_datatype typ) in
  if r then
    (* let () = print_endline "recursive" in *)
    let pendings = List.map make_pending l in
    (* let () = print_endline "make pending" in *)
    let typtab = List.fold_left add_binding typtab pendings in
    (* let () = print_endline "add pending to typtab" in *)
    let pendings = List.rev_map (from_binding typtab) l in
    (* let () = print_endline "from binding" in *)
    let rec iter typtab completes pendings = function
    | 0 -> (typtab, completes, pendings)
    | n ->
      (* let () = print_endline ("iter pending " ^ string_of_int n) in *)
      if List.length pendings = 0 then (typtab, completes, pendings) else
      let bindings = List.map (update_binding typtab) pendings in
      let (pendings, bindings) = List.partition filter_binding bindings in
      let typtab = List.fold_left add_binding typtab pendings in
      let bindings = List.map make_alias bindings in
      let typtab = List.fold_left add_binding typtab bindings in
      iter typtab (completes @ bindings) pendings (n-1)
    in
    let n = List.length pendings in
    let (typtab, completes, pendings) = iter typtab [] pendings n in
    if List.length pendings > 0 then
      raise Cyclic_types
    else
      let () = List.iter (resolve_binding typtab) completes in
      let () = List.iter (fun (_, t) -> check_datatype t) completes in
      let () = List.iter print completes in
      typtab
  else
    let aux typtab (binding : Ast.type_binding) = 
      let (name, typ) = make_alias (from_binding typtab binding) in
      let typtab = add_binding typtab (name, typ) in
      let () = check_datatype typ in
      let () = print (name, typ) in
      typtab
    in
    List.fold_left aux typtab l

and datatype_of_type_binding typtab ((params_opt, name, construct): Ast.type_binding) =
  (* let () = print_endline ("from type binding: " ^ name) in *)
  let typ = datatype_of_type_construct typtab construct in
  match params_opt with
  | None -> typ
  | Some params -> Generic (params, typ)

and datatype_of_type_construct typtab = function
  | Ast.TypeExpr t -> datatype_of_type_expr typtab t
  | Ast.VariantsType t -> datatype_of_variants_type typtab t
  | Ast.RecordType t -> datatype_of_record_type typtab t
  | Ast.InterfaceType t -> datatype_of_interface_type typtab t

and datatype_of_type_expr typtab =
  (* let () = print_endline ("from type expr") in *)
  function
  | Ast.SingleType expr ->
    (match expr with
     | Ast.TypeName name -> (lookup_type name typtab : datatype)
     | Ast.TypeSymbol id -> Param id)
  | Ast.TupleType l -> Tuple (List.map (datatype_of_type_expr typtab) l)
  | Ast.FunctionType (arg, ret) -> Function (datatype_of_type_expr typtab arg, datatype_of_type_expr typtab ret)
  | Ast.SpecificType (arg, name) -> Specific (datatype_of_type_expr typtab arg, lookup_type name typtab)

and datatype_of_variants_type typtab (l : Ast.variants_type) =
  (* let () = print_endline ("from variants type") in *)
  let from_variant = function
  | (name, None) -> (name, None)
  | (name, Some expr) -> (name, Some (ref (datatype_of_type_expr typtab expr)))
  in
  Variants (new_id (), (List.map from_variant l))

and datatype_of_record_type typtab (l : Ast.record_type) =
  (* let () = print_endline ("from record type") in *)
  let from_field (name, expr) = (name, ref (datatype_of_type_expr typtab expr)) in
  Record (new_id (), (List.map from_field l))

and datatype_of_interface_type typtab (l : Ast.interface_type) =
  (* let () = print_endline ("from interface type") in *)
  let from_method (name, expr) = (name, ref (datatype_of_type_expr typtab expr)) in
  Interface (List.map from_method l)


(* Match destination type with source type in a value binding *)
exception Type_mismatch

type mapping = (string * datatype) list

let rec agree_on_type typtab dest src =
  match dest, src with
  | Alias (name, d), s -> Alias (name, agree_on_type typtab d s)
  | d, Alias (name, s) -> agree_on_type typtab d s
  | Unit, Unit -> Unit
  | Bool, Bool -> Bool
  | Int, Int -> Int
  | Char, Char -> Char
  | String, String -> String
  | Ref, Ref -> Ref
  | Array, Array -> Array
  | d, Param _ -> d
  | Param _, s -> s
  | Tuple d, Tuple s -> agree_on_tuple typtab d s
  | Variants (did, d), Variants (sid, s) -> agree_on_variants typtab (did, d) (sid, s)
  | Record (did, d), Record (sid, s) -> agree_on_record typtab (did, d) (sid, s)
  | Interface d, s -> agree_on_interface typtab d s
  | Function (darg, dbody), Function (sarg, sbody) -> agree_on_function typtab (darg, dbody) (sarg, sbody)
  | Specific (darg, dbody), Specific (sarg, sbody) -> agree_on_specific typtab (darg, dbody) (sarg, sbody)
  | _ -> failwith "agree_on_type"

and agree_on_tuple typtab dest src =
  let mapping = constraints_to_mapping (List.combine dest src) in
  Tuple (List.map (apply_to_type mapping) dest)

and agree_on_variants typtab (did, dest) (sid, src) =
  if did = sid then Variants (did, dest)
  else raise Type_mismatch

and agree_on_record typtab (did, dest) (sid, src) =
  if did = sid then Record (did, dest)
  else raise Type_mismatch

and agree_on_interface typtab dest src =
  failwith "agree_on_interface"

and agree_on_function typtab (darg, dbody) (sarg, sbody) =
  let mapping = constraints_to_mapping [(darg, sarg); (dbody, sbody)] in
  Function (apply_to_type mapping darg, apply_to_type mapping dbody)

and agree_on_specific typtab (darg, dbody) (sarg, sbody) =
  failwith "agree_on_specific"

and constraints_to_mapping l : mapping =
  let aux acc x = acc @ extract_relations x in
  let relations = List.fold_left aux [] l in
  relations_to_mapping relations

and extract_relations = function
  | Unit, Unit -> [(Unit, Unit)]
  | _ -> failwith "extract_relations"

and relations_to_mapping l =
  failwith "relations_to_mapping"

and apply_to_type relations = function
  | Unit -> Unit
  | _ -> failwith "apply_to_type"


let datatype_of_params = function
  | [] -> failwith "datatype_of_params"
  | [x] -> Param x
  | l -> Tuple (List.map (fun x -> Param x) l)

exception Arity_mismatch

let rec walk_value_bindings typtab (r, l) =
  typtab

and walk_value_binding typtab ((pattern, expr) : Ast.value_binding) =
  Unit

and datatype_of_expr typtab = function
  | Ast.Unit -> Unit
  | Ast.Bool _ -> Bool
  | Ast.Int _ -> Int
  | Ast.Char _ -> Char
  | Ast.String _ -> String
  | Ast.CapIdent name -> datatype_of_capident typtab name
  | Ast.Variable v -> datatype_of_variable typtab v
  | Ast.Assign (v, e) -> datatype_of_assign typtab (v, e)
  | Ast.Tuple l -> Tuple (List.map (datatype_of_expr typtab) l)
  | Ast.List l -> datatype_of_list typtab l
  | Ast.Array l -> datatype_of_array typtab l
  | Ast.Record l -> datatype_of_record typtab l
  | Ast.Call (c, e) -> datatype_of_call typtab (c, e)
  | Ast.Unary (op, e) -> datatype_of_unary typtab (op, e)
  | Ast.Binary (op, a, b) -> datatype_of_binary typtab (op, a, b)
  | Ast.Local (b, e) -> datatype_of_local typtab (b, e)
  | Ast.IfExpr e -> datatype_of_if_expr typtab e
  | Ast.MatchExpr e -> datatype_of_match_expr typtab e
  | Ast.LambdaExpr e -> datatype_of_lambda_expr typtab e
  | Ast.ExprList l -> List.fold_left (fun x y -> y) Unit (List.map (datatype_of_expr typtab) l)
  | Ast.ExprWithType (e, t) -> agree_on_type typtab (datatype_of_type_expr typtab t) (datatype_of_expr typtab e)

and datatype_of_capident typtab name =
  let make_params param_names =
    let rec aux = function
    | [] -> []
    | h :: t ->
      if param_is_bound h typtab
      then Param (new_param typtab) :: aux t
      else Param h :: aux t
    in
    match aux param_names with
    | [] -> failwith "datatype_of_capident: make_params"
    | [x] -> x
    | l -> Tuple l
  in
  let (variant, typ) = lookup_constructor name typtab in
  match typ with
  | Alias (_, Generic (param_names, _)) ->
    let params = make_params param_names in
    (match variant with
     | (name, None) -> Specific (params, typ)
     | (name, Some arg) -> Function (!arg, Specific (params, typ)))
  | t -> 
    (match variant with
     | (name, None) -> t
     | (name, Some arg) -> Function (!arg, t))

and datatype_of_variable typtab = function
  | (None, name) -> lookup_symbol name typtab
  | _ -> failwith "datatype_of_variable: not implemented"

and datatype_of_assign typtab (v, e) =
  match v with
  | (None, name) ->
    let assignee = lookup_symbol name typtab in
    let assigner = datatype_of_expr typtab e in
    (match assignee with
     | Specific (param, typ) when typ = ref_type ->
       let _ = agree_on_type typtab param assigner in Unit
     | _ -> failwith "datatype_of_assign")
  | _ -> failwith "datatype_of_assign"

and datatype_of_list typtab l =
  let type_list = List.map (datatype_of_expr typtab) l in
  let typ = List.fold_left (agree_on_type typtab) (Param "a") type_list in
  Specific (typ, list_type)

and datatype_of_array typtab l =
  let type_list = List.map (datatype_of_expr typtab) l in
  let typ = List.fold_left (agree_on_type typtab) (Param "a") type_list in
  Specific (typ, array_type)

and datatype_of_record typtab l =
  failwith "datatype_of_record"

and datatype_of_call typtab (c, e) =
  let caller = datatype_of_expr typtab c in
  let callee = datatype_of_expr typtab e in
  match caller with
  | Function (arg, body) -> let _ = agree_on_type typtab arg callee in body
  | _ -> raise Type_mismatch

and datatype_of_unary typtab (op, e) =
  let operand = datatype_of_expr typtab e in
  match op with
  | Ast.Positive | Ast.Negative ->
    let _ = agree_on_type typtab Int operand in Int
  | Ast.Deref -> 
    match operand with
    | Specific (_, typ) when typ = ref_type -> typ
    | _ -> raise Type_mismatch

and datatype_of_binary typtab (op, a, b) =
  let a = datatype_of_expr typtab a in
  let b = datatype_of_expr typtab b in
  match op with
  | Ast.Plus | Ast.Minus | Ast.Times | Ast.Div | Ast.Mod ->
    let _ = agree_on_type typtab Int a in
    let _ = agree_on_type typtab Int b in
    Int
  | Ast.Lt | Ast.Lte | Ast.Gt | Ast.Gte | Ast.Eq | Ast.Neq ->
    let _ = agree_on_type typtab a b in Bool
  | Ast.And | Ast.Or ->
    let _ = agree_on_type typtab Bool a in
    let _ = agree_on_type typtab Bool b in
    Bool
  | Ast.Cons ->
    let typ = agree_on_type typtab (Param "a") a in
    let typ = Specific (typ, lookup_type "list" typtab) in
    let typ = agree_on_type typtab typ b in
    typ
  | Ast.Append ->
    let specific_list = Specific (Param "a", lookup_type "list" typtab) in
    let typ = agree_on_type typtab specific_list a in
    let typ = agree_on_type typtab typ b in
    typ
  | Ast.Concat ->
    let _ = agree_on_type typtab String a in
    let _ = agree_on_type typtab String b in
    String

and datatype_of_local typtab (bindings, e) =
  let typtab = walk_value_bindings typtab bindings in
  datatype_of_expr typtab e

and datatype_of_if_expr typtab = function
  | (cond, then_e, None) ->
    let cond = datatype_of_expr typtab cond in
    let then_t = datatype_of_expr typtab then_e in
    let _ = agree_on_type typtab Bool cond in
    let _ = agree_on_type typtab Unit then_t in
    Unit
  | (cond, then_e, Some else_e) ->
    let cond = datatype_of_expr typtab cond in
    let then_t = datatype_of_expr typtab then_e in
    let else_t = datatype_of_expr typtab else_e in
    let _ = agree_on_type typtab Bool cond in
    let typ = agree_on_type typtab then_t else_t in
    typ

and datatype_of_match_expr typtab (e, l) =
  failwith "datatype_of_match_expr"

and datatype_of_lambda_expr typtab l =
  failwith "datatype_of_lambda_expr"


let rec walk_method_bindings typtab (r, l) =
  failwith "walk_method_bindings"

let walk_decl typtab = function
  | Ast.TypeBindings b -> walk_type_bindings typtab b
  | Ast.ValueBindings b -> walk_value_bindings typtab b
  | Ast.MethodBindings b -> walk_method_bindings typtab b

let walk_decl_list l =
  let typtab = default_typtab in
  List.fold_left walk_decl typtab l