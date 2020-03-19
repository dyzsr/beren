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
  constructors : variant Scope.t;
  fields : field Scope.t;
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
exception Unbound_field of string

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
let default_typtab =
  let list_type =
    let ref_type = ref (Tuple [Param "a"; Specific (Param "a", Pending "list")]) in
    let variants = Variants (new_id (),
      [("[]", None);
       ("(::)", Some ref_type)]) in
    let alias = Alias ("list", Generic(["a"], variants)) in
    ref_type := Tuple [Param "a"; Specific (Param "a", alias)];
    alias
  in
  let ref_type = Alias ("ref", Generic (["a"], Ref)) in
  let array_type = Alias ("array", Generic (["a"], Array)) in
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
  { types = default_types;
    symbols = Scope.empty ();
    constructors = Scope.empty ();
    fields = Scope.empty (); }


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
  Tuple (agree_on_constraints typtab (List.combine dest src))

and agree_on_variants typtab (did, dest) (sid, src) =
  if did = sid then Variants (did, dest)
  else raise Type_mismatch

and agree_on_record typtab (did, dest) (sid, src) =
  if did = sid then Record (did, dest)
  else raise Type_mismatch

and agree_on_interface typtab dest src =
  failwith "agree_on_interface"

and agree_on_function typtab (darg, dbody) (sarg, sbody) =
  match agree_on_constraints typtab [(darg, sarg); (dbody, sbody)] with
  | [arg; body] -> Function (arg, body)
  | _ -> failwith "agree_on_function"

and agree_on_specific typtab (darg, dbody) (sarg, sbody) =
  failwith "agree_on_specific"

and agree_on_constraints typtab l =
  failwith "agree_on_constraints"


(* Make built-in types *)
let make_list_type (l : Ast.expr list) =
  failwith "make_list_type"

let make_array_type (l : Ast.expr list) =
  failwith "make_array_type"

let rec walk_value_bindings typtab (r, l) =
  typtab

and datatype_of_value_binding typtab ((pattern, expr) : Ast.value_binding) =
  Unit

and datatype_of_expr typtab = function
  | Ast.Unit -> Unit
  | Ast.Bool _ -> Bool
  | Ast.Int _ -> Int
  | Ast.Char _ -> Char
  | Ast.String _ -> String
  | Ast.Variable v -> datatype_of_variable typtab v
  | Ast.Assign (v, e) -> check_assign typtab (v, e); Unit
  | Ast.Tuple l -> Tuple (List.map (datatype_of_expr typtab) l)
  | _ -> failwith "datatype_of_expr"
  (* | Ast.List l -> expr list
  | Ast.Array l -> expr list
  | Ast.Record l -> record_expr
  | Ast.Call (c, e) -> expr * expr
  | Ast.Construct (name, e) -> string * expr
  | Ast.Unary (op, e) -> unary_op * expr
  | Ast.Binary (op, a, b) -> binary_op * expr * expr
  | Ast.Local (b, e) -> value_bindings * expr
  | Ast.IfExpr e -> if_expr
  | Ast.MatchExpr e -> match_expr
  | Ast.LambdaExpr e -> lambda_expr
  | Ast.ExprList l -> expr list
  | Ast.ExprWithType (e, t) -> expr * type_expr *)

and datatype_of_variable typtab = function
  | Ast.Expr (None, name) -> lookup_symbol name typtab
  | _ -> failwith "datatype_of_variable: not implemented"

and check_assign typtab (v, e) =
  failwith "check_assign"

let rec walk_method_bindings typtab (r, l) =
  typtab

let walk_decl typtab = function
  | Ast.TypeBindings b -> walk_type_bindings typtab b
  | Ast.ValueBindings b -> walk_value_bindings typtab b
  | Ast.MethodBindings b -> walk_method_bindings typtab b

let walk_decl_list l =
  let typtab = default_typtab in
  List.fold_left walk_decl typtab l