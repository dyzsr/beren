open Utils

let {new_id=new_typvar_id} = make_idgen 0
let {new_id=new_variants_id} = make_idgen 0
let {new_id=new_record_id} = make_idgen 0
let {new_id=new_generic_id} = make_idgen 0

(* Data types *)
type datatype =
  | Unit
  | Bool
  | Int
  | Char
  | String
  | Ref
  | Array
  | Typvar of typvar
  | Tuple of datatype list
  | Variants of id * variant list (* variants *)
  | Record of id * field list (* fields *)
  | Interface of signature list
  | Function of datatype * datatype
  | Generic of id * typvar list * datatype
  | Specific of datatype * datatype
  | Alias of string * datatype (* a complete type with an alias *)
  | Pending of string (* an incomplete type with only a name *)

(* Type variable *)
and typvar = string * id

and variant = string * datatype ref option
and field = string (* name *) * datatype ref
and signature = string * datatype ref

let new_typvar name =
  (name, new_typvar_id ())

module String_map = Map.Make (String)
module Int_map = Map.Make (Int)

(* let empty_string_map () = String_map.empty *)

(* make an outer scope with a default value map *)
let make_string_map defaults : 'a String_map.t =
  String_map.of_seq (List.to_seq defaults)

(* Symbol table *)
type symtab =
  { types : datatype String_map.t
  ; values : datatype String_map.t
  ; constructors : (variant * datatype) String_map.t
  ; fields : (field * datatype) String_map.t
  }

exception Unbound_type of string

let lookup_type name {types=types} = 
  match String_map.find_opt name types with
  | None -> raise (Unbound_type name)
  | Some typ -> typ

let add_type name typ ({types=types} as symtab) =
  let types = String_map.add name typ types in
  {symtab with types=types}

exception Unbound_value of string

let lookup_value name {values=values} =
  match String_map.find_opt name values with
  | None -> raise (Unbound_value name)
  | Some sym -> sym
  
let add_value name sym ({values=values} as symtab) =
  let values = String_map.add name sym values in
  {symtab with values=values}
  
exception Unbound_constructor of string

let lookup_constructor name {constructors=constructors} =
  match String_map.find_opt name constructors with
  | None -> raise (Unbound_constructor name)
  | Some con -> con

let add_constructor name con ({constructors=constructors} as symtab) =
  let constructors = String_map.add name con constructors in
  {symtab with constructors=constructors}

exception Unbound_field of string

let lookup_field name {fields=fields} =
  match String_map.find_opt name fields with
  | None -> raise (Unbound_field name)
  | Some field -> field

let add_field name field ({fields=fields} as symtab) =
  let fields = String_map.add name field fields in
  {symtab with fields=fields}

(* Type variables table *)
type tvtab =
  { tvmap : id String_map.t (* name -> datatype *)
  ; idmap : datatype Int_map.t (* id -> datatype *)
  }

let empty_tvtab =
  { tvmap = String_map.empty
  ; idmap = Int_map.empty
  }

(* type idmap = datatype Int_map.t *)
(* type tvmap = datatype String_map.t *)

exception Unbound_type_variable of string
exception Duplicate_type_variable of string

(* let empty_tvmap = String_map.empty *)

let bind_typvar name key ({tvmap} as tvtab) =
  let update = function
  | None -> Some key
  | Some id when id = key -> Some key
  | Some _ -> raise (Duplicate_type_variable name)
  in
  let tvmap = String_map.update name update tvmap in
  { tvtab with tvmap }

let lookup_typvar name {tvmap} =
  match String_map.find_opt name tvmap with
  | None -> raise (Unbound_type_variable name)
  | Some typvar -> typvar

let lookup_typvar_opt name {tvmap} =
  String_map.find_opt name tvmap

let next_free_typvar key tvtab =
  let rec iter ch count =
    let name =
      if count = 0 then String.make 1 ch
      else String.make 1 ch ^ string_of_int count
    in
    match lookup_typvar_opt name tvtab with
    | None -> name
    | Some id when id = key -> name
    | Some _ ->
      if ch = 'z' then iter 'a' (count + 1)
      else iter (char_of_int (int_of_char ch + 1)) count
  in
  iter 'a' 0

(* let empty_idmap = Int_map.empty *)

let rec find_id key tvtab =
  (* let () = print_endline ("find_tvtab: " ^ string_of_int key) in *)
  let typvar = Typvar ("a", key) in
  match lookup_id key tvtab with
  | None -> (* found nothing *)
    let tvtab = add_id key typvar tvtab in (* update *)
    tvtab, typvar
  | Some (Typvar (_, id) as t) when id = key -> (* found itself *)
    tvtab, t
  | Some (Typvar (_, id)) -> (* found other type variable *)
    let tvtab, t = find_id id tvtab in (* find root *)
    let tvtab = add_id key t tvtab in (* update the path to root *)
    tvtab, t
  | Some t -> (* found a datatype other than type variable *)
    tvtab, t

and add_id key typ ({idmap} as tvtab) =
  let idmap = Int_map.add key typ idmap in
  { tvtab with idmap }

and lookup_id key {idmap} =
  Int_map.find_opt key idmap


(* Stringify *)
let rec string_of_toplevel_datatype = function
  | Alias (name, Generic (_, typvars, typ)) ->
    let fold tvtab (name, id) =
      bind_typvar name id tvtab
    in
    let string_of_typvars = function
    | [(name, id)] -> ("'" ^ name)
    | l ->
      let typvars = List.map (fun (name, _) -> "'" ^ name) l in
      "(" ^ String.concat ", " typvars ^ ")"
    in
    let tvmap = List.fold_left fold empty_tvtab typvars in
    let typvars_string = string_of_typvars typvars in
    (match typ with
     | Ref -> "type " ^ typvars_string ^ " ref"
     | Array -> "type " ^ typvars_string ^ " array"
     | _ ->
       let _, str = string_of_datatype tvmap 0 typ in
       "type " ^ typvars_string ^ " " ^ name ^ " = " ^ str)
  | Alias (name, typ) ->
    let _, str = string_of_datatype empty_tvtab 0 typ in
    "type " ^ name ^ " = " ^ str
  | t ->
    let _, str = string_of_datatype empty_tvtab 0 t in
    "type _ = " ^ str

and enclose p1 p2 name =
  if p1 < p2 then "(" ^ name ^ ")" else name

and string_of_datatype tvmap priority = function
  | Unit -> tvmap, "unit"
  | Bool -> tvmap, "bool"
  | Int -> tvmap, "int"
  | Char -> tvmap, "char"
  | String -> tvmap, "string"
  | Ref -> failwith "string_of_datatype: ref"
  | Array -> failwith "string_of_datatype: array"
  | Typvar (name, key) ->
    let tvmap, name =
      match lookup_typvar_opt name tvmap with
      | None ->
        let tvmap = bind_typvar name key tvmap in
        tvmap, name
      | Some _ ->
        let name = next_free_typvar key tvmap in
        let tvmap = bind_typvar name key tvmap in
        tvmap, name
    in
    tvmap, "'(" ^ name ^ ", " ^ string_of_int key ^ ")"
  | Tuple l ->
    let rec iter tvmap acc = function
    | [] -> tvmap, acc
    | typ :: l ->
      let tvmap, str = string_of_datatype tvmap 20 typ in
      iter tvmap (str :: acc) l
    in
    let tvmap, strs = iter tvmap [] l in
    tvmap, enclose 20 priority (String.concat " * " (List.rev strs))
  | Variants (_, l) ->
    let rec iter tvmap acc = function
    | [] -> tvmap, acc
    | variant :: l ->
      let tvmap, str = match variant with
      | (name, None) -> tvmap, name
      | (name, Some typ) ->
        let tvmap, str = string_of_datatype tvmap 0 (!typ) in
        tvmap, (name ^ " of " ^ str)
      in
      iter tvmap (str :: acc) l
    in
    let tvmap, strs = iter tvmap [] l in
    tvmap, String.concat " | " (List.rev strs)
  | Record (_, l) ->
    let rec iter tvmap acc = function
    | [] -> tvmap, acc
    | (name, typ) :: l ->
      let tvmap, str = string_of_datatype tvmap 0 (!typ) in
      iter tvmap ((name ^ " : " ^ str) :: acc) l
    in
    let tvmap, strs = iter tvmap [] l in
    tvmap, "{ " ^ String.concat "; " (List.rev strs) ^ " }"
  | Interface l ->
    let rec iter tvmap acc = function
    | [] -> tvmap, acc
    | (name, typ) :: l ->
      let tvmap, str = string_of_datatype tvmap 0 (!typ) in
      iter tvmap ((name ^ " : " ^ str) :: acc) l
    in
    let tvmap, strs = iter tvmap [] l in
    tvmap, "sig " ^ String.concat "; " (List.rev strs) ^ " end"
  | Function (arg, body) ->
    let tvmap, arg = string_of_datatype tvmap 11 arg in
    let tvmap, body = string_of_datatype tvmap 10 body in
    tvmap, enclose 10 priority (arg ^ " -> " ^ body)
  | Generic (id, params, body) ->
    tvmap, ("generic: " ^ string_of_int id)
  | Specific (arg, body) ->
    let tvmap, arg = string_of_datatype tvmap 30 arg in
    let tvmap, body = string_of_datatype tvmap 0 body in
    tvmap, arg ^ " " ^ body
  | Alias (name, _) -> tvmap, name
  | Pending name -> tvmap, "[" ^ name ^ "]"

let print_type name typ =
  print_endline (string_of_toplevel_datatype typ)

let print_value name typ =
  let _, typ = string_of_datatype empty_tvtab 0 typ in
  print_endline ("val " ^ name ^ " : " ^ typ)

let print_value2 name typ1 typ2 =
  let _, typ1 = string_of_datatype empty_tvtab 0 typ1 in
  let _, typ2 = string_of_datatype empty_tvtab 0 typ2 in
  print_endline ("val " ^ name ^ " : " ^ typ1 ^ " --- " ^ typ2)

let print_symtab symtab =
  String_map.iter print_type symtab.types;
  String_map.iter print_value symtab.values;
  ()

(* Update pending types in tuple, function, and generics *)
let rec update_pending symtab = function
  | Unit | Bool | Int | Char | String | Ref | Array as t -> t
  | Typvar _ as t -> t
  | Tuple l -> Tuple (List.map (update_pending symtab) l)
  | Variants _ as t -> t
  | Record _ as t -> t
  | Interface _ as t -> t
  | Function (arg, body) -> Function (update_pending symtab arg, update_pending symtab body)
  | Generic (id, params, body) -> Generic (id, params, update_pending symtab body)
  | Specific (arg, body) -> Specific (update_pending symtab arg, update_pending symtab body)
  | Alias _ as t -> t
  | Pending name -> lookup_type name symtab

(* Resolve pending types inside variants, record, and interface *)
let rec resolve_pending symtab = function
  | Unit | Bool | Int | Char | String | Ref | Array | Typvar _ | Alias _ -> ()
  | Tuple l -> List.iter (resolve_pending symtab) l
  | Variants (_, l) ->
    let (resolve_variant : variant -> unit) = function
    | (_, None) -> ()
    | (_, Some typ) -> typ := update_pending symtab (!typ)
    in
    List.iter resolve_variant l
  | Record (_, l) ->
    let resolve_field (_, typ : field) =
      typ := update_pending symtab (!typ)
    in
    List.iter resolve_field l
  | Interface l ->
    let resolve_signature (_, typ : signature) =
      typ := update_pending symtab (!typ)
    in
    List.iter resolve_signature l
  | Function (arg, body) -> resolve_pending symtab arg; resolve_pending symtab body
  | Generic (_, _, body) -> resolve_pending symtab body
  | Specific (arg, body) -> resolve_pending symtab arg; resolve_pending symtab body
  | Pending _ -> failwith "resolve_pending"

(* Filter types with pending inside *)
let rec filter_pending = function
  | Unit | Bool | Int | Char | String | Ref | Array | Typvar _ -> false
  | Tuple l ->
    let rec iter = function
    | [] -> false
    | h :: t -> if filter_pending h then true else iter t
    in
    iter l
  | Variants _ | Record _ | Interface _ -> false
  | Function (arg, body) -> filter_pending arg || filter_pending body
  | Generic (_, _, body) -> filter_pending body
  | Specific (arg, body) -> filter_pending arg || filter_pending body
  | Alias (_, typ) -> false
  | Pending _ -> true


let rec extract_alias = function
  | Alias (_, typ) -> typ
  | t -> t

exception Type_arity_mismatch of string

let rec check_datatype = function
  | Unit | Bool | Int | Char | String -> ()
  | Ref -> failwith "check_datatype: ref"
  | Array -> failwith "check_datatype: array"
  | Typvar _ -> ()
  | Tuple l -> List.iter check_datatype l
  | Variants (_, l) -> let aux = function
    | (_, None) -> ()
    | (_, Some t) -> check_datatype (!t)
    in
    List.iter aux l
  | Record (_, l) -> List.iter (fun (_, t) -> check_datatype (!t)) l
  | Interface l -> List.iter (fun (_, t) -> check_datatype (!t)) l
  | Function (arg, body) -> check_datatype arg; check_datatype body
  | Generic (_, _, Ref) | Generic (_, _, Array) -> ()
  | Generic (_, _, body) -> check_datatype body
  | Specific (arg, body) -> check_datatype arg;
    (match body with
      | Alias (name, Generic _) -> ()
      | Alias (name, _) -> raise (Type_arity_mismatch name)
      | _ -> failwith "check_datatype: specific")
  | Alias (_, Alias (name, Generic _)) -> raise (Type_arity_mismatch name)
  | Alias _ -> ()
  | Pending _ -> failwith "check_datatype: pending"


(* Default type table for access *)

(* define list *)
let list_type =
  let typvar_for_list = new_typvar "a" in
  let ref_to_cons =
    let typvar = Typvar typvar_for_list in
    ref (Tuple [typvar; Specific (typvar, Pending "list")])
  in
  let list_nil = ("[]", None) in
  let list_cons = ("(::)", Some ref_to_cons) in
  let variants = Variants (new_typvar_id (), [list_nil; list_cons]) in
  let alias = Alias ("list", Generic (new_typvar_id (), [typvar_for_list], variants)) in
  let typvar = Typvar typvar_for_list in
  ref_to_cons := Tuple [typvar; Specific (typvar, alias)];
  alias

let list_nil, list_cons =
  match list_type with
  | Alias ("list", Generic (_, _, Variants (_, [nil; cons]))) -> nil, cons
  | _ -> failwith "list_type"

(* define ref *)
let ref_type = Alias ("ref", Generic (new_typvar_id (), [new_typvar "a"], Ref))

(* define array *)
let array_type = Alias ("array", Generic (new_typvar_id (), [new_typvar "a"], Array))

(* type table *)
let default_symtab =
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
  let default_types = make_string_map default_type_list in
  (* let print (_, typ) = print_endline (string_of_toplevel_datatype typ) in *)
  (* let () = List.iter print default_type_list in *)
  (* save default values *)
  let default_value_list =
    let typvar = Typvar (new_typvar "a") in
    [("ref", Function (typvar, Specific (typvar, ref_type)))] in
  let default_values = make_string_map default_value_list in
  (* save default constructors *)
  let default_constructor_list =
    [("[]", (list_nil, list_type));
     ("(::)", (list_cons, list_type))]
  in
  let default_constructors = make_string_map default_constructor_list in
  (* create the default type table *)
  { types = default_types
  ; values = default_values
  ; constructors = default_constructors
  ; fields = String_map.empty
  }


(* Walk the AST to perform type inference and checking *)

exception Cyclic_types
exception Same_constructors_name of string

(* get constructor names from type definitions
  @consmap : constructors map
  @typ : toplevel type definition *)
let get_constructors (consmap : (variant * datatype) String_map.t) typ =
  let fold dt consmap ((name, x) as variant) =
    let update = function
    | None -> Some (variant, dt)
    | Some _ -> raise (Same_constructors_name name)
    in
    String_map.update name update consmap
  in
  match typ with
  | Alias (_, Generic (_, _, Variants (id, l))) as dt ->
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
  String_map.merge merge map1 map2

let rec walk_type_bindings symtab (r, l) = 
  let make_pending ((_, name, _) : Ast.type_binding) =
    (name, Pending name) in
  let make_alias (name, typ) =
    (name, Alias (name, typ)) in
  let from_binding symtab ((_, name, _) as binding : Ast.type_binding) =
    (* let () = print_endline ("from binding " ^ name) in *)
    (name, datatype_of_type_binding symtab binding) in
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
  let consmap = List.fold_left get_constructors String_map.empty typs in
  let constructors = merge_string_maps symtab.constructors consmap in
  { symtab with constructors=constructors }

and datatype_of_type_binding symtab ((params_opt, name, construct): Ast.type_binding) =
  (* let () = print_endline ("from type binding: " ^ name) in *)
  let tvtab = empty_tvtab in
  match params_opt with
  | None -> 
    datatype_of_type_construct symtab tvtab construct
  | Some typvars ->
    let typvars = List.map (fun x -> new_typvar x) typvars in
    let update_tvtab tvtab (name, id) =
      bind_typvar name id tvtab in
    let tvtab = List.fold_left update_tvtab tvtab typvars in
    let typ = datatype_of_type_construct symtab tvtab construct in
    Generic (new_generic_id (), typvars, typ)

and datatype_of_type_construct symtab tvtab = function
  | Ast.TypeExpr t -> datatype_of_type_expr symtab tvtab t
  | Ast.VariantsType t -> datatype_of_variants_type symtab tvtab t
  | Ast.RecordType t -> datatype_of_record_type symtab tvtab t
  | Ast.InterfaceType t -> datatype_of_interface_type symtab tvtab t

and datatype_of_type_op symtab tvtab is_decl =
  (* let () = print_endline ("from type expr") in *)
  function
  | Ast.SingleType expr ->
    (match expr with
    | Ast.TypeName name ->
      tvtab, (lookup_type name symtab : datatype)
    | Ast.TypeSymbol name ->
      if is_decl then
        tvtab, Typvar (name, lookup_typvar name tvtab)
      else
        match lookup_typvar_opt name tvtab with
        | None ->
          let id = new_typvar_id () in
          let tvtab = bind_typvar name id tvtab in
          tvtab, Typvar (name, id)
        | Some id ->
          tvtab, Typvar (name, id)
    )
  | Ast.TupleType l ->
    let rec iter tvtab acc = function
    | [] -> tvtab, acc
    | x :: l ->
      let tvtab, typ = datatype_of_type_op symtab tvtab is_decl x in
      iter tvtab (typ :: acc) l
    in
    let tvtab, typs = iter tvtab [] l in
    tvtab, Tuple (List.rev typs)
  | Ast.FunctionType (arg, body) ->
    let tvtab, arg = datatype_of_type_op symtab tvtab is_decl arg in
    let tvtab, body = datatype_of_type_op symtab tvtab is_decl body in
    tvtab, Function (arg, body)
  | Ast.SpecificType (arg, name) ->
    let tvtab, arg = datatype_of_type_op symtab tvtab is_decl arg in
    tvtab, Specific (arg, lookup_type name symtab)

and datatype_of_type_expr symtab tvtab t =
  let _, typ = datatype_of_type_op symtab tvtab true t in
  typ
  (* let () = print_endline ("from type expr") in *)
  (* function
  | Ast.SingleType expr ->
    (match expr with
    | Ast.TypeName name -> (lookup_type name symtab : datatype)
    | Ast.TypeSymbol name -> Typvar (name, lookup_typvar name tvtab)
    )
  | Ast.TupleType l ->
    Tuple (List.map (datatype_of_type_expr symtab tvtab) l)
  | Ast.FunctionType (arg, ret) ->
    Function (datatype_of_type_expr symtab tvtab arg, datatype_of_type_expr symtab tvtab ret)
  | Ast.SpecificType (arg, name) ->
    Specific (datatype_of_type_expr symtab tvtab arg, lookup_type name symtab) *)

and datatype_of_type_annotation symtab tvtab t =
  datatype_of_type_op symtab tvtab false t

and datatype_of_variants_type symtab tvtab (l : Ast.variants_type) =
  (* let () = print_endline ("from variants type") in *)
  let from_variant = function
  | (name, None) -> (name, None)
  | (name, Some expr) -> (name, Some (ref (datatype_of_type_expr symtab tvtab expr)))
  in
  Variants (new_variants_id (), (List.map from_variant l))

and datatype_of_record_type symtab tvtab (l : Ast.record_type) =
  (* let () = print_endline ("from record type") in *)
  let from_field (name, expr) = (name, ref (datatype_of_type_expr symtab tvtab expr)) in
  Record (new_record_id (), (List.map from_field l))

and datatype_of_interface_type symtab tvtab (l : Ast.interface_type) =
  (* let () = print_endline ("from interface type") in *)
  let from_method (name, expr) = (name, ref (datatype_of_type_expr symtab tvtab expr)) in
  Interface (List.map from_method l)


(* Match destination type with source type in a value binding *)
exception Type_mismatch
exception Incompatible_types of string * string

let rec apply_to_type tvtab = function
  | Unit | Bool | Int | Char | String | Ref | Array as t -> t
  | Typvar (_, key) ->
    let tvtab, t = find_id key tvtab in
    (match t with
    | Typvar (_, id) when id = key -> t
    | _ -> apply_to_type tvtab t
    )
  | Tuple l ->
    Tuple (List.map (apply_to_type tvtab) l)
  | Variants _ | Record _ | Interface _ as t -> t
  | Function (arg, body) ->
    Function (apply_to_type tvtab arg, apply_to_type tvtab body)
  | Generic _ as t -> t
  | Specific (arg, body) ->
    Specific (apply_to_type tvtab arg, body)
  | Alias (name, typ) ->
    Alias (name, apply_to_type tvtab typ)
  | t ->
    let _, msg = string_of_datatype empty_tvtab 0 t in
    failwith ("apply_to_type: " ^ msg)

let rec agree_on_type tvtab dest src =
  (* let () =
    print_value2 "~dest" dest (apply_to_type tvtab dest);
    print_value2 "~src" src (apply_to_type tvtab src)
  in *)
  match dest, src with
  | Alias (name, d), s -> agree_on_type tvtab d s
  | d, Alias (name, s) -> agree_on_type tvtab d s

  | Unit, Unit | Bool, Bool | Int, Int | Char, Char | String, String
  | Ref, Ref | Array, Array -> tvtab

  (* union-find *)
  | Typvar (_, id1), Typvar (_, id2) ->
    let tvtab, t1 = find_id id1 tvtab in
    let tvtab, t2 = find_id id2 tvtab in
    let tvtab = (match t1, t2 with
    | Typvar (_, root1), Typvar (_, root2) when root1 = root2 ->
      tvtab
    | t1, Typvar (_, root2) ->
      add_id root2 t1 tvtab
    | Typvar (_, root1), t2 ->
      add_id root1 t2 tvtab
    | t1, t2 ->
      agree_on_type tvtab t1 t2
    ) in
    (* let () = print_value2 "**merged**" (apply_to_type tvtab t1) (apply_to_type tvtab t2) in *)
    tvtab
  | d, Typvar (_, id) ->
    (* let () = print_endline "agree_on_type: dest type_var" in *)
    let tvtab, t = find_id id tvtab in
    (match t with
    | Typvar (_, root) -> add_id root d tvtab
    | _ -> agree_on_type tvtab d t
    )
  | Typvar (_, id), s ->
    (* let () = print_endline "agree_on_type: type_var src" in *)
    let tvtab, t = find_id id tvtab in
    (match t with
    | Typvar (_, root) -> add_id root s tvtab
    | _ -> agree_on_type tvtab t s
    )

  | Tuple d, Tuple s ->
    List.fold_left2 agree_on_type tvtab d s
  | Variants (did, d), Variants (sid, s) ->
    if did = sid then tvtab else raise Type_mismatch
  | Record (did, d), Record (sid, s) ->
    if did = sid then tvtab else raise Type_mismatch
  | Interface d, s ->
    failwith "agree_on_interface"
  | Function (darg, dbody), Function (sarg, sbody) ->
    let tvtab = agree_on_type tvtab darg sarg in
    agree_on_type tvtab dbody sbody
  | Generic (did, dvars, dbody), Generic (sid, svars, sbody) ->
    agree_on_generics tvtab (did, dvars, dbody) (sid, svars, sbody)
  | Specific (darg, dbody), Specific (sarg, sbody) ->
    let tvtab = agree_on_type tvtab darg sarg in
    agree_on_type tvtab dbody sbody
  | d, s ->
    try
      let _, d = string_of_datatype empty_tvtab 0 d in
      let _, s = string_of_datatype empty_tvtab 0 s in
      raise (Incompatible_types (d, s)) 
    with
    | Failure msg -> failwith ("agree_on_type: " ^ msg)

and agree_on_generics tvtab (did, _, _) (sid, _, _) =
  if did = sid then tvtab else raise Type_mismatch


(* Extract types from value bindings *)
exception Arity_mismatch

let fold_symtab symtab (k, v) =
  let () = print_value k v in
  add_value k v symtab

let make_typvars (typvars : typvar list) =
  let idmapping = List.map (fun (name, id) -> id, Typvar (new_typvar name)) typvars in
  let idmap = Int_map.of_seq (List.to_seq idmapping) in
  let tvtab = {empty_tvtab with idmap} in
  let _, typvars = List.split idmapping in
  match typvars with
  | [] -> failwith "datatype_of_capident: make_typvars"
  | [x] -> x, tvtab
  | l -> Tuple l, tvtab

let rec substitute_typvars tvtab = function
  | Unit | Bool | Int | Char | String | Ref | Array as t -> t
  | Typvar (_, id) ->
    (match lookup_id id tvtab with
     | Some t -> t
     | None -> failwith "substitute_typvars: id is not found")
  | Tuple l -> Tuple (List.map (substitute_typvars tvtab) l)
  | Variants _ | Record _ | Interface _ | Generic _ as t -> t
  | Function (arg, body) -> Function (substitute_typvars tvtab arg, substitute_typvars tvtab body)
  | Specific (arg, body) -> Specific (substitute_typvars tvtab arg, body)
  | Alias (name, typ) -> Alias (name, substitute_typvars tvtab typ)
  | _ -> failwith "substitute_typvars"

let rec walk_value_bindings symtab tvtab (r, l) =
  if r then (* recursive *)
    (* add bindings to the symbol table before getting real types *)
    let rec init_bindings symtab tvtab acc_pattern_types acc_bindings = function
    | [] -> symtab, tvtab, acc_pattern_types, acc_bindings
    | (pattern, _) :: l ->
      let bindings, tvtab, typ = walk_pattern symtab tvtab pattern in
      let symtab = List.fold_left fold_symtab symtab bindings in
      init_bindings symtab tvtab (typ :: acc_pattern_types) (bindings @ acc_bindings) l
    in
    let symtab, tvtab, pattern_types, bindings = init_bindings symtab tvtab [] [] l in
    let pattern_types = List.rev pattern_types in
    (* fill the bindings with real types *)
    let rec fill_bindings tvtab = function
    | [] -> tvtab
    | (pattern_typ, (_, expr)) :: l ->
      let tvtab, expr_typ = datatype_of_expr symtab tvtab expr in
      let tvtab = agree_on_type tvtab pattern_typ expr_typ in
      let () =
        print_value "[pattern]" (apply_to_type tvtab pattern_typ);
        print_value "[expr]" (apply_to_type tvtab expr_typ)
      in
      fill_bindings tvtab l
    in
    let tvtab = fill_bindings tvtab (List.combine pattern_types l) in
    let bindings = List.map (fun (k, v) -> k, apply_to_type tvtab v) bindings in
    let symtab = List.fold_left fold_symtab symtab bindings in
    symtab, tvtab
  else (* non-recursive *)
    let walk_value_binding (pattern, expr) =
      let bindings, tvtab, pattern_typ = walk_pattern symtab tvtab pattern in
      let tvtab, expr_typ = datatype_of_expr symtab tvtab expr in
      let tvtab = agree_on_type tvtab pattern_typ expr_typ in
      let () =
        print_value "[pattern]" (apply_to_type tvtab pattern_typ);
        print_value "[expr]" (apply_to_type tvtab expr_typ)
      in
      List.map (fun (k, v) -> k, apply_to_type tvtab v) bindings
    in
    let bindings = List.fold_left (@) [] (List.map walk_value_binding l) in
    List.fold_left fold_symtab symtab bindings, tvtab

and datatype_of_expr symtab tvtab = function
  | Ast.Unit -> tvtab, Unit
  | Ast.Bool _ -> tvtab, Bool
  | Ast.Int _ -> tvtab, Int
  | Ast.Char _ -> tvtab, Char
  | Ast.String _ -> tvtab, String
  | Ast.CapIdent name -> datatype_of_capident symtab tvtab name
  | Ast.Variable v -> datatype_of_variable symtab tvtab v
  | Ast.Assign (v, e) -> datatype_of_assign symtab tvtab (v, e)
  | Ast.Tuple l ->
    let rec iter tvtab acc = function
    | [] -> tvtab, acc
    | e :: l ->
      let tvtab, typ = datatype_of_expr symtab tvtab e in
      iter tvtab (typ :: acc) l
    in
    let tvtab, typs = iter tvtab [] l in
    tvtab, Tuple (List.rev typs)
  | Ast.List l -> datatype_of_list symtab tvtab l
  | Ast.Array l -> datatype_of_array symtab tvtab l
  | Ast.Record l -> datatype_of_record symtab tvtab l
  | Ast.Call (c, e) -> datatype_of_call symtab tvtab (c, e)
  | Ast.Unary (op, e) -> datatype_of_unary symtab tvtab (op, e)
  | Ast.Binary (op, a, b) -> datatype_of_binary symtab tvtab (op, a, b)
  | Ast.Local (b, e) -> datatype_of_local symtab tvtab (b, e)
  | Ast.IfExpr e -> datatype_of_if_expr symtab tvtab e
  | Ast.MatchExpr e -> datatype_of_match_expr symtab tvtab e
  | Ast.LambdaExpr (arg, body) -> datatype_of_lambda_expr symtab tvtab (arg, body)
  | Ast.ExprList l ->
    let rec iter tvtab acc = function
    | [] -> tvtab, acc
    | e :: l ->
      let tvtab, typ = datatype_of_expr symtab tvtab e in
      iter tvtab typ l
    in
    iter tvtab Unit l
  | Ast.ExprWithType (e, t) ->
    let tvtab, e_typ = datatype_of_expr symtab tvtab e in
    let tvab, t_typ = datatype_of_type_annotation symtab tvtab t in
    let tvtab = agree_on_type tvtab t_typ e_typ in
    tvtab, t_typ

and datatype_of_capident symtab tvtab name =
  let variant, typ = lookup_constructor name symtab in
  let typ =
    match typ with
    | Alias (_, Generic (_, var_names, _)) ->
      let typvars, varmap = make_typvars var_names in
      (match variant with
      | name, None -> Specific (typvars, typ)
      | name, Some arg ->
        let arg = substitute_typvars varmap !arg in
        Function (arg, Specific (typvars, typ))
      )
    | t -> 
      (match variant with
      | name, None -> t
      | name, Some arg -> Function (!arg, t)
      )
  in
  let () = print_value name typ in
  tvtab, typ

and datatype_of_variable symtab tvtab = function
  | None, name ->
    let var = lookup_value name symtab in
    let () = print_value2 ("$" ^ name) var (apply_to_type tvtab var) in
    tvtab, var
  | _ ->
    failwith "datatype_of_variable: not implemented"

and datatype_of_assign symtab tvtab (v, e) =
  let tvtab, assignee = datatype_of_variable symtab tvtab v in
  let tvtab, assigner = datatype_of_expr symtab tvtab e in
  (match assignee with
   | Specific (typvar, typ) when typ = ref_type ->
     let tvtab = agree_on_type tvtab typvar assigner in
     tvtab, Unit
   | _ -> failwith "datatype_of_assign")

and datatype_of_list symtab tvtab l =
  let tvtab, typ = datatype_of_item_list symtab tvtab l in
  tvtab, Specific (typ, list_type)

and datatype_of_array symtab tvtab l =
  let tvtab, typ = datatype_of_item_list symtab tvtab l in
  tvtab, Specific (typ, array_type)

and datatype_of_item_list symtab tvtab l =
  let acc_typ = Typvar (new_typvar "a") in
  let aux tvtab e =
    let tvtab, typ = datatype_of_expr symtab tvtab e in
    agree_on_type tvtab acc_typ typ
  in
  let tvtab = List.fold_left aux tvtab l in
  tvtab, acc_typ

and datatype_of_record symtab l =
  failwith "datatype_of_record"

and datatype_of_call symtab tvtab (c, e) =
  let tvtab, caller = datatype_of_expr symtab tvtab c in
  let tvtab, callee = datatype_of_expr symtab tvtab e in
  let arg = Typvar (new_typvar "a") in
  let body = Typvar (new_typvar "a") in
  let func_type = Function (arg, body) in
  let tvtab = agree_on_type tvtab func_type caller in
  let tvtab = agree_on_type tvtab arg callee in
  let () = print_value2 "[caller]" func_type (apply_to_type tvtab func_type) in
  let () = print_value2 "[callee]" arg (apply_to_type tvtab arg) in
  tvtab, body

and datatype_of_unary symtab tvtab (op, e) =
  let tvtab, operand = datatype_of_expr symtab tvtab e in
  match op with
  | Ast.Positive | Ast.Negative ->
    let tvtab = agree_on_type tvtab Int operand in
    tvtab, Int
  | Ast.Deref -> 
    let typ = Typvar (new_typvar "a") in
    let ref_typ = Specific (typ, ref_type) in
    let tvtab = agree_on_type tvtab ref_typ operand in
    tvtab, typ

and datatype_of_binary symtab tvtab (op, a, b) =
  let tvtab, a = datatype_of_expr symtab tvtab a in
  let tvtab, b = datatype_of_expr symtab tvtab b in
  match op with
  | Ast.Plus | Ast.Minus | Ast.Times | Ast.Div | Ast.Mod ->
    let tvtab = agree_on_type tvtab Int a in
    let tvtab = agree_on_type tvtab Int b in
    tvtab, Int
  | Ast.Lt | Ast.Lte | Ast.Gt | Ast.Gte | Ast.Eq | Ast.Neq ->
    let tvtab = agree_on_type tvtab a b in
    tvtab, Bool
  | Ast.And | Ast.Or ->
    let tvtab = agree_on_type tvtab Bool a in
    let tvtab = agree_on_type tvtab Bool b in
    tvtab, Bool
  | Ast.Cons ->
    let list_typ = Specific (a, lookup_type "list" symtab) in
    let tvtab = agree_on_type tvtab list_typ b in
    tvtab, list_typ
  | Ast.Append ->
    let list_typ = Specific (Typvar (new_typvar "a"), lookup_type "list" symtab) in
    let tvtab = agree_on_type tvtab list_typ a in
    let tvtab = agree_on_type tvtab list_typ b in
    tvtab, list_typ
  | Ast.Concat ->
    let tvtab = agree_on_type tvtab String a in
    let tvtab = agree_on_type tvtab String b in
    tvtab, String

and datatype_of_local symtab tvtab (bindings, e) =
  let symtab, tvtab = walk_value_bindings symtab tvtab bindings in
  datatype_of_expr symtab tvtab e

and datatype_of_if_expr symtab tvtab (cond, then_e, else_opt) =
  let tvtab, cond = datatype_of_expr symtab tvtab cond in
  let tvtab = agree_on_type tvtab Bool cond in
  let tvtab, then_t = datatype_of_expr symtab tvtab then_e in
  match else_opt with
  | None ->
    let tvtab = agree_on_type tvtab Unit then_t in
    tvtab, Unit
  | Some else_e ->
    let tvtab, else_t = datatype_of_expr symtab tvtab else_e in
    let tvtab = agree_on_type tvtab then_t else_t in
    tvtab, then_t

and datatype_of_match_expr symtab tvtab (e, l) =
  let tvtab, acc_pattern = datatype_of_expr symtab tvtab e in
  let acc_body = Typvar (new_typvar "a") in
  let () = print_value "[body]" acc_body in
  let aux tvtab (pattern, body) =
    let bindings, tvtab, pattern = walk_pattern symtab tvtab pattern in
    let tvtab = agree_on_type tvtab acc_pattern pattern in
    let () = print_value2 "[branch-pattern]" pattern (apply_to_type tvtab pattern) in
    let bindings = List.map (fun (k, v) -> k, apply_to_type tvtab v) bindings in
    let symtab = List.fold_left fold_symtab symtab bindings in
    let tvtab, body = datatype_of_expr symtab tvtab body in
    let tvtab = agree_on_type tvtab acc_body body in
    let () = print_value2 "[branch-body]" body (apply_to_type tvtab body) in
    tvtab
  in
  let tvtab = List.fold_left aux tvtab l in
  let typ = acc_body in
  let () = print_value2 "[match]" typ (apply_to_type tvtab typ) in
  tvtab, typ

and datatype_of_lambda_expr symtab tvtab (arg, body) =
  let bindings, tvtab, arg = walk_pattern symtab tvtab arg in
  let bindings = List.map (fun (k, v) -> k, apply_to_type tvtab v) bindings in
  let symtab = List.fold_left fold_symtab symtab bindings in
  let tvtab, body = datatype_of_expr symtab tvtab body in
  let func = Function (arg, body) in
  let () = print_value2 "[fun]" func (apply_to_type tvtab func) in
  tvtab, func

and walk_pattern symtab tvtab = function
  | Ast.BoolPattern b -> [], tvtab, Bool
  | Ast.IntPattern i -> [], tvtab, Int
  | Ast.CharPattern c -> [], tvtab, Char
  | Ast.StringPattern s -> [], tvtab, String
  | Ast.UnitPattern -> [], tvtab, Unit
  | Ast.TuplePattern l ->
    let rec iter bindings tvtab acc = function
    | [] -> bindings, tvtab, acc
    | p :: l ->
      let new_bindings, tvtab, typ = walk_pattern symtab tvtab p in
      iter (new_bindings @ bindings) tvtab (typ :: acc) l
    in
    let bindings, tvtab, typs = iter [] tvtab [] l in
    (List.rev bindings), tvtab, Tuple (List.rev typs)
  | Ast.ConsPattern (hd, tl) ->
    walk_variant_pattern symtab tvtab ("(::)", Some (Ast.TuplePattern [hd; tl]))
  | Ast.ListPattern l ->
    let bindings, tvtab, typ = walk_pattern_item_list symtab tvtab l in
    bindings, tvtab, Specific (typ, list_type)
  | Ast.ArrayPattern l ->
    let bindings, tvtab, typ = walk_pattern_item_list symtab tvtab l in
    bindings, tvtab, Specific (typ, array_type)
  | Ast.RecordPattern l ->
    walk_record_pattern symtab tvtab l
  | Ast.RefPattern p ->
    let bindings, tvtab, typ = walk_pattern symtab tvtab p in
    bindings, tvtab, Specific (typ, ref_type)
  | Ast.VariablePattern name ->
    let typ = Typvar (new_typvar "a") in
    let () = print_value ("<" ^ name ^ ">") typ in
    let binding = (name, typ) in
    [binding], tvtab, typ
  | Ast.Wildcard ->
    let typ = Typvar (new_typvar "a") in
    let () = print_value "_" typ in
    [], tvtab, typ
  | Ast.VariantsPattern (name, p) ->
    walk_variant_pattern symtab tvtab (name, p)
  | Ast.PatternList l ->
    walk_pattern_list symtab tvtab l
  | Ast.PatternWithType (p, t) ->
    let bindings, tvtab, p_typ = walk_pattern symtab tvtab p in
    let tvtab, t_typ = datatype_of_type_annotation symtab tvtab t in
    let tvtab = agree_on_type tvtab t_typ p_typ in
    bindings, tvtab, t_typ

and walk_pattern_item_list symtab tvtab l =
  let acc_typ = Typvar (new_typvar "a") in
  let rec iter bindings tvtab = function
  | [] -> bindings, tvtab
  | p :: l ->
    let new_bindings, tvtab, typ = walk_pattern symtab tvtab p in
    let tvtab = agree_on_type tvtab acc_typ typ in
    iter (new_bindings @ bindings) tvtab l
  in
  let bindings, tvtab = iter [] tvtab l in
  (List.rev bindings), tvtab, acc_typ

and walk_record_pattern symtab tvtab l = 
  failwith "walk_record_pattern"

and walk_variant_pattern symtab tvtab (name, pattern_opt) =
  let variant, typ = lookup_constructor name symtab in
  match typ with
  | Alias (_, Generic (_, typvars, t)) ->
    let typvars, varmap = make_typvars typvars in
    (match pattern_opt with
     | None -> 
       [], tvtab, Specific (typvars, typ)
     | Some p ->
       let bindings, tvtab, pattern_typ = walk_pattern symtab tvtab p in
       let variant_typ = match variant with
         | (name, None) -> raise (Type_arity_mismatch name)
         | (name, Some {contents=typ}) -> substitute_typvars varmap typ
       in
       let tvtab = agree_on_type tvtab variant_typ pattern_typ in
       bindings, tvtab, Specific (typvars, typ))
  | _ ->
    [], tvtab, typ


and walk_pattern_list symtab tvtab l =
  let acc_typ = Typvar (new_typvar "a") in
  let rec iter bindings_list tvtab = function
  | [] -> bindings_list, tvtab
  | p :: l ->
    let bindings, tvtab, typ = walk_pattern symtab tvtab p in
    let tvtab = agree_on_type tvtab acc_typ typ in
    iter (bindings :: bindings_list) tvtab l
  in
  let bindings_list, tvtab = iter [] tvtab l in
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
  bindings, tvtab, acc_typ

let rec walk_method_bindings symtab (r, l) =
  failwith "walk_method_bindings"

let walk_decl symtab = function
  | Ast.TypeBindings b ->
    walk_type_bindings symtab b
  | Ast.ValueBindings b ->
    let () = print_endline "# Walk value declaration" in
    let symtab, _ = walk_value_bindings symtab empty_tvtab b in
    symtab
  | Ast.MethodBindings b ->
    walk_method_bindings symtab b

let walk_decl_list l =
  let symtab = default_symtab in
  List.fold_left walk_decl symtab l