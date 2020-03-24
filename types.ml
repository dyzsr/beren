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

(* Type tables *)
module String_map = Map.Make (String)

let empty () = String_map.empty

(* make an outer scope with a default value map *)
let make defaults : 'a String_map.t =
  String_map.of_seq (List.to_seq defaults)

type symtab = {
  types : datatype String_map.t;
  values : datatype String_map.t;
  constructors : (variant * datatype) String_map.t;
  fields : (field * datatype) String_map.t;
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
type tvtab = datatype String_map.t

exception Unbound_type_varaible of string
exception Duplicate_type_variable of string

let empty_tvtab = String_map.empty

let bind_typvar name key tvtab =
  let update = function
  | None -> Some key
  | Some id when id = key -> Some key
  | Some _ -> raise (Duplicate_type_variable name)
  in
  String_map.update name update tvtab

let lookup_typvar name tvtab =
  match String_map.find_opt name tvtab with
  | None -> raise (Unbound_type_varaible name)
  | Some typvar -> typvar

let lookup_typvar_opt name tvtab =
  String_map.find_opt name tvtab

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
    let tvtab = List.fold_left fold String_map.empty typvars in
    let typvars_string = string_of_typvars typvars in
    (match typ with
     | Ref -> "type " ^ typvars_string ^ " ref"
     | Array -> "type " ^ typvars_string ^ " array"
     | _ ->
       let _, str = string_of_datatype tvtab 0 typ in
       "type " ^ typvars_string ^ " " ^ name ^ " = " ^ str)
  | Alias (name, typ) ->
    let _, str = string_of_datatype String_map.empty 0 typ in
    "type " ^ name ^ " = " ^ str
  | t ->
    let _, str = string_of_datatype String_map.empty 0 t in
    "type _ = " ^ str

and enclose p1 p2 name =
  if p1 < p2 then "(" ^ name ^ ")" else name

and string_of_datatype tvtab priority = function
  | Unit -> tvtab, "unit"
  | Bool -> tvtab, "bool"
  | Int -> tvtab, "int"
  | Char -> tvtab, "char"
  | String -> tvtab, "string"
  | Ref -> failwith "string_of_datatype: ref"
  | Array -> failwith "string_of_datatype: array"
  | Typvar (name, key) ->
    let tvtab, name =
      match lookup_typvar_opt name tvtab with
      | None ->
        let tvtab = bind_typvar name key tvtab in
        tvtab, name
      | Some _ ->
        let name = next_free_typvar key tvtab in
        let tvtab = bind_typvar name key tvtab in
        tvtab, name
    in
    tvtab, "'(" ^ name ^ ", " ^ string_of_int key ^ ")"
  | Tuple l ->
    let rec iter tvtab acc = function
    | [] -> tvtab, acc
    | typ :: l ->
      let tvtab, str = string_of_datatype tvtab 20 typ in
      iter tvtab (str :: acc) l
    in
    let tvtab, strs = iter tvtab [] l in
    tvtab, enclose 20 priority (String.concat " * " (List.rev strs))
  | Variants (_, l) ->
    let rec iter tvtab acc = function
    | [] -> tvtab, acc
    | variant :: l ->
      let tvtab, str = match variant with
      | (name, None) -> tvtab, name
      | (name, Some typ) ->
        let tvtab, str = string_of_datatype tvtab 0 (!typ) in
        tvtab, (name ^ " of " ^ str)
      in
      iter tvtab (str :: acc) l
    in
    let tvtab, strs = iter tvtab [] l in
    tvtab, String.concat " | " (List.rev strs)
  | Record (_, l) ->
    let rec iter tvtab acc = function
    | [] -> tvtab, acc
    | (name, typ) :: l ->
      let tvtab, str = string_of_datatype tvtab 0 (!typ) in
      iter tvtab ((name ^ " : " ^ str) :: acc) l
    in
    let tvtab, strs = iter tvtab [] l in
    tvtab, "{ " ^ String.concat "; " (List.rev strs) ^ " }"
  | Interface l ->
    let rec iter tvtab acc = function
    | [] -> tvtab, acc
    | (name, typ) :: l ->
      let tvtab, str = string_of_datatype tvtab 0 (!typ) in
      iter tvtab ((name ^ " : " ^ str) :: acc) l
    in
    let tvtab, strs = iter tvtab [] l in
    tvtab, "sig " ^ String.concat "; " (List.rev strs) ^ " end"
  | Function (arg, body) ->
    let tvtab, arg = string_of_datatype tvtab 11 arg in
    let tvtab, body = string_of_datatype tvtab 10 body in
    tvtab, enclose 10 priority (arg ^ " -> " ^ body)
  | Generic (id, params, body) ->
    tvtab, ("generic: " ^ string_of_int id)
  | Specific (arg, body) ->
    let tvtab, arg = string_of_datatype tvtab 30 arg in
    let tvtab, body = string_of_datatype tvtab 0 body in
    tvtab, arg ^ " " ^ body
  | Alias (name, _) -> tvtab, name
  | Pending name -> tvtab, "[" ^ name ^ "]"

let print_type name typ =
  print_endline (string_of_toplevel_datatype typ)

let print_value name typ =
  let _, typ = string_of_datatype String_map.empty 0 typ in
  print_endline ("val " ^ name ^ " : " ^ typ)

let print_value2 name typ1 typ2 =
  let _, typ1 = string_of_datatype String_map.empty 0 typ1 in
  let _, typ2 = string_of_datatype String_map.empty 0 typ2 in
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
  let default_types = make default_type_list in
  (* let print (_, typ) = print_endline (string_of_toplevel_datatype typ) in *)
  (* let () = List.iter print default_type_list in *)
  (* save default values *)
  let default_value_list =
    let typvar = Typvar (new_typvar "a") in
    [("ref", Function (typvar, Specific (typvar, ref_type)))] in
  let default_values = make default_value_list in
  (* save default constructors *)
  let default_constructor_list =
    [("[]", (list_nil, list_type));
     ("(::)", (list_cons, list_type))]
  in
  let default_constructors = make default_constructor_list in
  (* create the default type table *)
  { types = default_types;
    values = default_values;
    constructors = default_constructors;
    fields = empty (); }


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
  let tvtab = empty () in
  match params_opt with
  | None -> 
    datatype_of_type_construct symtab tvtab construct
  | Some typvars ->
    let typvars = List.map (fun x -> new_typvar x) typvars in
    let update_tvtab tvtab (name, id) =
      bind_typvar name (Typvar (name, id)) tvtab in
    let tvtab = List.fold_left update_tvtab tvtab typvars in
    let typ = datatype_of_type_construct symtab tvtab construct in
    Generic (new_generic_id (), typvars, typ)

and datatype_of_type_construct symtab tvtab = function
  | Ast.TypeExpr t -> datatype_of_type_expr symtab tvtab t
  | Ast.VariantsType t -> datatype_of_variants_type symtab tvtab t
  | Ast.RecordType t -> datatype_of_record_type symtab tvtab t
  | Ast.InterfaceType t -> datatype_of_interface_type symtab tvtab t

and datatype_of_type_expr symtab tvtab =
  (* let () = print_endline ("from type expr") in *)
  function
  | Ast.SingleType expr ->
    (match expr with
     | Ast.TypeName name -> (lookup_type name symtab : datatype)
     | Ast.TypeSymbol name -> lookup_typvar name tvtab)
  | Ast.TupleType l ->
    Tuple (List.map (datatype_of_type_expr symtab tvtab) l)
  | Ast.FunctionType (arg, ret) ->
    Function (datatype_of_type_expr symtab tvtab arg, datatype_of_type_expr symtab tvtab ret)
  | Ast.SpecificType (arg, name) ->
    Specific (datatype_of_type_expr symtab tvtab arg, lookup_type name symtab)

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

(* Type variables map: id -> datatype *)
module Int_map = Map.Make (Int)
type idmap = datatype Int_map.t

let (empty_idmap : idmap) = Int_map.empty

let rec find_idmap key (idmap : idmap) =
  (* let () = print_endline ("find_idmap: " ^ string_of_int key) in *)
  let typvar = Typvar ("a", key) in
  match lookup_idmap key idmap with
  | None -> (* found nothing *)
    let idmap = add_idmap key typvar idmap in (* update *)
    idmap, typvar
  | Some (Typvar (_, id) as t) when id = key -> (* found itself *)
    idmap, t
  | Some (Typvar (_, id)) -> (* found other type variable *)
    let idmap, t = find_idmap id idmap in (* find root *)
    let idmap = add_idmap key t idmap in (* update the path to root *)
    idmap, t
  | Some t -> (* found a datatype other than type variable *)
    idmap, t

and add_idmap key typ (idmap : idmap) =
  Int_map.add key typ idmap

and lookup_idmap key (idmap : idmap) =
  Int_map.find_opt key idmap


exception Incompatible_types of string * string

let rec apply_to_type idmap = function
  | Unit | Bool | Int | Char | String | Ref | Array as t -> t
  | Typvar (_, key) ->
    let idmap, t = find_idmap key idmap in
    (match t with
    | Typvar (_, id) when id = key -> t
    | _ -> apply_to_type idmap t
    )
  | Tuple l ->
    Tuple (List.map (apply_to_type idmap) l)
  | Variants _ | Record _ | Interface _ as t -> t
  | Function (arg, body) ->
    Function (apply_to_type idmap arg, apply_to_type idmap body)
  | Generic _ as t -> t
  | Specific (arg, body) ->
    Specific (apply_to_type idmap arg, body)
  | Alias (name, typ) ->
    Alias (name, apply_to_type idmap typ)
  | t ->
    let _, msg = string_of_datatype String_map.empty 0 t in
    failwith ("apply_to_type: " ^ msg)

let rec agree_on_type idmap dest src =
  (* let () =
    print_value2 "~dest" dest (apply_to_type idmap dest);
    print_value2 "~src" src (apply_to_type idmap src)
  in *)
  match dest, src with
  | Alias (name, d), s -> agree_on_type idmap d s
  | d, Alias (name, s) -> agree_on_type idmap d s

  | Unit, Unit | Bool, Bool | Int, Int | Char, Char | String, String
  | Ref, Ref | Array, Array -> idmap

  (* union-find *)
  | Typvar (_, id1), Typvar (_, id2) ->
    let idmap, t1 = find_idmap id1 idmap in
    let idmap, t2 = find_idmap id2 idmap in
    let idmap = (match t1, t2 with
    | Typvar (_, root1), Typvar (_, root2) when root1 = root2 ->
      idmap
    | t1, Typvar (_, root2) ->
      add_idmap root2 t1 idmap
    | Typvar (_, root1), t2 ->
      add_idmap root1 t2 idmap
    | t1, t2 ->
      agree_on_type idmap t1 t2
    ) in
    (* let () = print_value2 "**merged**" (apply_to_type idmap t1) (apply_to_type idmap t2) in *)
    idmap
  | d, Typvar (_, id) ->
    (* let () = print_endline "agree_on_type: dest type_var" in *)
    let idmap, t = find_idmap id idmap in
    (match t with
    | Typvar (_, root) -> add_idmap root d idmap
    | _ -> agree_on_type idmap d t
    )
  | Typvar (_, id), s ->
    (* let () = print_endline "agree_on_type: type_var src" in *)
    let idmap, t = find_idmap id idmap in
    (match t with
    | Typvar (_, root) -> add_idmap root s idmap
    | _ -> agree_on_type idmap t s
    )

  | Tuple d, Tuple s ->
    List.fold_left2 agree_on_type idmap d s
  | Variants (did, d), Variants (sid, s) ->
    if did = sid then idmap else raise Type_mismatch
  | Record (did, d), Record (sid, s) ->
    if did = sid then idmap else raise Type_mismatch
  | Interface d, s ->
    failwith "agree_on_interface"
  | Function (darg, dbody), Function (sarg, sbody) ->
    let idmap = agree_on_type idmap darg sarg in
    agree_on_type idmap dbody sbody
  | Generic (did, dvars, dbody), Generic (sid, svars, sbody) ->
    agree_on_generics idmap (did, dvars, dbody) (sid, svars, sbody)
  | Specific (darg, dbody), Specific (sarg, sbody) ->
    let idmap = agree_on_type idmap darg sarg in
    agree_on_type idmap dbody sbody
  | d, s ->
    try
      let _, d = string_of_datatype empty_tvtab 0 d in
      let _, s = string_of_datatype empty_tvtab 0 s in
      raise (Incompatible_types (d, s)) 
    with
    | Failure msg -> failwith ("agree_on_type: " ^ msg)

and agree_on_generics idmap (did, _, _) (sid, _, _) =
  if did = sid then idmap else raise Type_mismatch


(* Extract types from value bindings *)
exception Arity_mismatch

let fold_symtab symtab (k, v) =
  let () = print_value k v in
  add_value k v symtab

let make_typvars (typvars : typvar list) =
  let idmapping = List.map (fun (name, id) -> id, Typvar (new_typvar name)) typvars in
  let idmap = Int_map.of_seq (List.to_seq idmapping) in
  let _, typvars = List.split idmapping in
  match typvars with
  | [] -> failwith "datatype_of_capident: make_typvars"
  | [x] -> x, idmap
  | l -> Tuple l, idmap

let rec substitute_typvars idmap = function
  | Unit | Bool | Int | Char | String | Ref | Array as t -> t
  | Typvar (_, id) ->
    (match lookup_idmap id idmap with
     | Some t -> t
     | None -> failwith "substitute_typvars: id is not found")
  | Tuple l -> Tuple (List.map (substitute_typvars idmap) l)
  | Variants _ | Record _ | Interface _ | Generic _ as t -> t
  | Function (arg, body) -> Function (substitute_typvars idmap arg, substitute_typvars idmap body)
  | Specific (arg, body) -> Specific (substitute_typvars idmap arg, body)
  | Alias (name, typ) -> Alias (name, substitute_typvars idmap typ)
  | _ -> failwith "substitute_typvars"

let rec walk_value_bindings symtab idmap (r, l) =
  if r then (* recursive *)
    (* add bindings to the symbol table before getting real types *)
    let rec init_bindings symtab idmap acc_pattern_types acc_bindings = function
    | [] -> symtab, idmap, acc_pattern_types, acc_bindings
    | (pattern, _) :: l ->
      let bindings, idmap, typ = walk_pattern symtab idmap pattern in
      let symtab = List.fold_left fold_symtab symtab bindings in
      init_bindings symtab idmap (typ :: acc_pattern_types) (bindings @ acc_bindings) l
    in
    let symtab, idmap, pattern_types, bindings = init_bindings symtab idmap [] [] l in
    let pattern_types = List.rev pattern_types in
    (* fill the bindings with real types *)
    let rec fill_bindings idmap = function
    | [] -> idmap
    | (pattern_typ, (_, expr)) :: l ->
      let idmap, expr_typ = datatype_of_expr symtab idmap expr in
      let idmap = agree_on_type idmap pattern_typ expr_typ in
      let () =
        print_value "[pattern]" (apply_to_type idmap pattern_typ);
        print_value "[expr]" (apply_to_type idmap expr_typ)
      in
      fill_bindings idmap l
    in
    let idmap = fill_bindings idmap (List.combine pattern_types l) in
    let bindings = List.map (fun (k, v) -> k, apply_to_type idmap v) bindings in
    let symtab = List.fold_left fold_symtab symtab bindings in
    symtab, idmap
  else (* non-recursive *)
    let walk_value_binding (pattern, expr) =
      let bindings, idmap, pattern_typ = walk_pattern symtab idmap pattern in
      let idmap, expr_typ = datatype_of_expr symtab idmap expr in
      let idmap = agree_on_type idmap pattern_typ expr_typ in
      let () =
        print_value "[pattern]" (apply_to_type idmap pattern_typ);
        print_value "[expr]" (apply_to_type idmap expr_typ)
      in
      List.map (fun (k, v) -> k, apply_to_type idmap v) bindings
    in
    let bindings = List.fold_left (@) [] (List.map walk_value_binding l) in
    List.fold_left fold_symtab symtab bindings, idmap

and datatype_of_expr symtab idmap = function
  | Ast.Unit -> idmap, Unit
  | Ast.Bool _ -> idmap, Bool
  | Ast.Int _ -> idmap, Int
  | Ast.Char _ -> idmap, Char
  | Ast.String _ -> idmap, String
  | Ast.CapIdent name -> datatype_of_capident symtab idmap name
  | Ast.Variable v -> datatype_of_variable symtab idmap v
  | Ast.Assign (v, e) -> datatype_of_assign symtab idmap (v, e)
  | Ast.Tuple l ->
    let rec iter idmap acc = function
    | [] -> idmap, acc
    | e :: l ->
      let idmap, typ = datatype_of_expr symtab idmap e in
      iter idmap (typ :: acc) l
    in
    let idmap, typs = iter idmap [] l in
    idmap, Tuple (List.rev typs)
  | Ast.List l -> datatype_of_list symtab idmap l
  | Ast.Array l -> datatype_of_array symtab idmap l
  | Ast.Record l -> datatype_of_record symtab idmap l
  | Ast.Call (c, e) -> datatype_of_call symtab idmap (c, e)
  | Ast.Unary (op, e) -> datatype_of_unary symtab idmap (op, e)
  | Ast.Binary (op, a, b) -> datatype_of_binary symtab idmap (op, a, b)
  | Ast.Local (b, e) -> datatype_of_local symtab idmap (b, e)
  | Ast.IfExpr e -> datatype_of_if_expr symtab idmap e
  | Ast.MatchExpr e -> datatype_of_match_expr symtab idmap e
  | Ast.LambdaExpr (arg, body) -> datatype_of_lambda_expr symtab idmap (arg, body)
  | Ast.ExprList l ->
    let rec iter idmap acc = function
    | [] -> idmap, acc
    | e :: l ->
      let idmap, typ = datatype_of_expr symtab idmap e in
      iter idmap typ l
    in
    iter idmap Unit l
  | Ast.ExprWithType (e, t) ->
    let idmap, e_typ = datatype_of_expr symtab idmap e in
    let t_typ = datatype_of_type_expr symtab empty_tvtab t in
    let idmap = agree_on_type idmap t_typ e_typ in
    idmap, t_typ

and datatype_of_capident symtab idmap name =
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
  idmap, typ

and datatype_of_variable symtab idmap = function
  | None, name ->
    let var = lookup_value name symtab in
    let () = print_value2 ("$" ^ name) var (apply_to_type idmap var) in
    idmap, var
  | _ ->
    failwith "datatype_of_variable: not implemented"

and datatype_of_assign symtab idmap (v, e) =
  let idmap, assignee = datatype_of_variable symtab idmap v in
  let idmap, assigner = datatype_of_expr symtab idmap e in
  (match assignee with
   | Specific (typvar, typ) when typ = ref_type ->
     let idmap = agree_on_type idmap typvar assigner in
     idmap, Unit
   | _ -> failwith "datatype_of_assign")

and datatype_of_list symtab idmap l =
  let idmap, typ = datatype_of_item_list symtab idmap l in
  idmap, Specific (typ, list_type)

and datatype_of_array symtab idmap l =
  let idmap, typ = datatype_of_item_list symtab idmap l in
  idmap, Specific (typ, array_type)

and datatype_of_item_list symtab idmap l =
  let acc_typ = Typvar (new_typvar "a") in
  let aux idmap e =
    let idmap, typ = datatype_of_expr symtab idmap e in
    agree_on_type idmap acc_typ typ
  in
  let idmap = List.fold_left aux idmap l in
  idmap, acc_typ

and datatype_of_record symtab l =
  failwith "datatype_of_record"

and datatype_of_call symtab idmap (c, e) =
  let idmap, caller = datatype_of_expr symtab idmap c in
  let idmap, callee = datatype_of_expr symtab idmap e in
  let arg = Typvar (new_typvar "a") in
  let body = Typvar (new_typvar "a") in
  let func_type = Function (arg, body) in
  let idmap = agree_on_type idmap func_type caller in
  let idmap = agree_on_type idmap arg callee in
  let () = print_value2 "[caller]" func_type (apply_to_type idmap func_type) in
  let () = print_value2 "[callee]" arg (apply_to_type idmap arg) in
  idmap, body

and datatype_of_unary symtab idmap (op, e) =
  let idmap, operand = datatype_of_expr symtab idmap e in
  match op with
  | Ast.Positive | Ast.Negative ->
    let idmap = agree_on_type idmap Int operand in
    idmap, Int
  | Ast.Deref -> 
    let typ = Typvar (new_typvar "a") in
    let ref_typ = Specific (typ, ref_type) in
    let idmap = agree_on_type idmap ref_typ operand in
    idmap, typ

and datatype_of_binary symtab idmap (op, a, b) =
  let idmap, a = datatype_of_expr symtab idmap a in
  let idmap, b = datatype_of_expr symtab idmap b in
  match op with
  | Ast.Plus | Ast.Minus | Ast.Times | Ast.Div | Ast.Mod ->
    let idmap = agree_on_type idmap Int a in
    let idmap = agree_on_type idmap Int b in
    idmap, Int
  | Ast.Lt | Ast.Lte | Ast.Gt | Ast.Gte | Ast.Eq | Ast.Neq ->
    let idmap = agree_on_type idmap a b in
    idmap, Bool
  | Ast.And | Ast.Or ->
    let idmap = agree_on_type idmap Bool a in
    let idmap = agree_on_type idmap Bool b in
    idmap, Bool
  | Ast.Cons ->
    let list_typ = Specific (a, lookup_type "list" symtab) in
    let idmap = agree_on_type idmap list_typ b in
    idmap, list_typ
  | Ast.Append ->
    let list_typ = Specific (Typvar (new_typvar "a"), lookup_type "list" symtab) in
    let idmap = agree_on_type idmap list_typ a in
    let idmap = agree_on_type idmap list_typ b in
    idmap, list_typ
  | Ast.Concat ->
    let idmap = agree_on_type idmap String a in
    let idmap = agree_on_type idmap String b in
    idmap, String

and datatype_of_local symtab idmap (bindings, e) =
  let symtab, idmap = walk_value_bindings symtab idmap bindings in
  datatype_of_expr symtab idmap e

and datatype_of_if_expr symtab idmap (cond, then_e, else_opt) =
  let idmap, cond = datatype_of_expr symtab idmap cond in
  let idmap = agree_on_type idmap Bool cond in
  let idmap, then_t = datatype_of_expr symtab idmap then_e in
  match else_opt with
  | None ->
    let idmap = agree_on_type idmap Unit then_t in
    idmap, Unit
  | Some else_e ->
    let idmap, else_t = datatype_of_expr symtab idmap else_e in
    let idmap = agree_on_type idmap then_t else_t in
    idmap, then_t

and datatype_of_match_expr symtab idmap (e, l) =
  let idmap, acc_pattern = datatype_of_expr symtab idmap e in
  let acc_body = Typvar (new_typvar "a") in
  let () = print_value "[body]" acc_body in
  let aux idmap (pattern, body) =
    let bindings, idmap, pattern = walk_pattern symtab idmap pattern in
    let idmap = agree_on_type idmap acc_pattern pattern in
    let () = print_value2 "[branch-pattern]" pattern (apply_to_type idmap pattern) in
    let bindings = List.map (fun (k, v) -> k, apply_to_type idmap v) bindings in
    let symtab = List.fold_left fold_symtab symtab bindings in
    let idmap, body = datatype_of_expr symtab idmap body in
    let idmap = agree_on_type idmap acc_body body in
    let () = print_value2 "[branch-body]" body (apply_to_type idmap body) in
    idmap
  in
  let idmap = List.fold_left aux idmap l in
  let typ = acc_body in
  let () = print_value2 "[match]" typ (apply_to_type idmap typ) in
  idmap, typ

and datatype_of_lambda_expr symtab idmap (arg, body) =
  let bindings, idmap, arg = walk_pattern symtab idmap arg in
  let bindings = List.map (fun (k, v) -> k, apply_to_type idmap v) bindings in
  let symtab = List.fold_left fold_symtab symtab bindings in
  let idmap, body = datatype_of_expr symtab idmap body in
  let func = Function (arg, body) in
  let () = print_value2 "[fun]" func (apply_to_type idmap func) in
  idmap, func

and walk_pattern symtab idmap = function
  | Ast.BoolPattern b -> [], idmap, Bool
  | Ast.IntPattern i -> [], idmap, Int
  | Ast.CharPattern c -> [], idmap, Char
  | Ast.StringPattern s -> [], idmap, String
  | Ast.UnitPattern -> [], idmap, Unit
  | Ast.TuplePattern l ->
    let rec iter bindings idmap acc = function
    | [] -> bindings, idmap, acc
    | p :: l ->
      let new_bindings, idmap, typ = walk_pattern symtab idmap p in
      iter (new_bindings @ bindings) idmap (typ :: acc) l
    in
    let bindings, idmap, typs = iter [] idmap [] l in
    (List.rev bindings), idmap, Tuple (List.rev typs)
  | Ast.ConsPattern (hd, tl) ->
    walk_variant_pattern symtab idmap ("(::)", Some (Ast.TuplePattern [hd; tl]))
  | Ast.ListPattern l ->
    let bindings, idmap, typ = walk_pattern_item_list symtab idmap l in
    bindings, idmap, Specific (typ, list_type)
  | Ast.ArrayPattern l ->
    let bindings, idmap, typ = walk_pattern_item_list symtab idmap l in
    bindings, idmap, Specific (typ, array_type)
  | Ast.RecordPattern l ->
    walk_record_pattern symtab idmap l
  | Ast.RefPattern p ->
    let bindings, idmap, typ = walk_pattern symtab idmap p in
    bindings, idmap, Specific (typ, ref_type)
  | Ast.VariablePattern name ->
    let typ = Typvar (new_typvar "a") in
    let () = print_value ("<" ^ name ^ ">") typ in
    let binding = (name, typ) in
    [binding], idmap, typ
  | Ast.Wildcard ->
    let typ = Typvar (new_typvar "a") in
    let () = print_value "_" typ in
    [], idmap, typ
  | Ast.VariantsPattern (name, p) ->
    walk_variant_pattern symtab idmap (name, p)
  | Ast.PatternList l ->
    walk_pattern_list symtab idmap l
  | Ast.PatternWithType (p, t) ->
    let bindings, idmap, p_typ = walk_pattern symtab idmap p in
    let t_typ = datatype_of_type_expr symtab empty_tvtab t in
    let idmap = agree_on_type idmap t_typ p_typ in
    bindings, idmap, t_typ

and walk_pattern_item_list symtab idmap l =
  let acc_typ = Typvar (new_typvar "a") in
  let rec iter bindings idmap = function
  | [] -> bindings, idmap
  | p :: l ->
    let new_bindings, idmap, typ = walk_pattern symtab idmap p in
    let idmap = agree_on_type idmap acc_typ typ in
    iter (new_bindings @ bindings) idmap l
  in
  let bindings, idmap = iter [] idmap l in
  (List.rev bindings), idmap, acc_typ

and walk_record_pattern symtab idmap l = 
  failwith "walk_record_pattern"

and walk_variant_pattern symtab idmap (name, pattern_opt) =
  let variant, typ = lookup_constructor name symtab in
  match typ with
  | Alias (_, Generic (_, typvars, t)) ->
    let typvars, varmap = make_typvars typvars in
    (match pattern_opt with
     | None -> 
       [], idmap, Specific (typvars, typ)
     | Some p ->
       let bindings, idmap, pattern_typ = walk_pattern symtab idmap p in
       let variant_typ = match variant with
         | (name, None) -> raise (Type_arity_mismatch name)
         | (name, Some {contents=typ}) -> substitute_typvars varmap typ
       in
       let idmap = agree_on_type idmap variant_typ pattern_typ in
       bindings, idmap, Specific (typvars, typ))
  | _ ->
    [], idmap, typ


and walk_pattern_list symtab idmap l =
  let acc_typ = Typvar (new_typvar "a") in
  let rec iter bindings_list idmap = function
  | [] -> bindings_list, idmap
  | p :: l ->
    let bindings, idmap, typ = walk_pattern symtab idmap p in
    let idmap = agree_on_type idmap acc_typ typ in
    iter (bindings :: bindings_list) idmap l
  in
  let bindings_list, idmap = iter [] idmap l in
  let compare (name1, _) (name2, _) =
    compare name1 name2
  in
  let bindings_list = List.map (List.sort compare) bindings_list in
  let rec iter idmap acc = function
  | [] -> idmap, acc
  | bindings :: l ->
    let bindings = List.combine acc bindings in
    let rec combine idmap acc = function
    | [] -> idmap, acc
    | ((acc_name, acc_typ), (binding_name, binding_typ)) :: l ->
      if acc_name <> binding_name then raise Type_mismatch else
      let idmap = agree_on_type idmap acc_typ binding_typ in
      combine idmap ((acc_name, acc_typ) :: acc) l
    in
    let idmap, bindings = combine idmap [] bindings in
    iter idmap bindings l
  in
  let idmap, bindings = 
    (match bindings_list with
    | [] -> failwith "walk_pattern_list"
    | bindings :: l -> iter idmap bindings l)
  in
  bindings, idmap, acc_typ

let rec walk_method_bindings symtab (r, l) =
  failwith "walk_method_bindings"

let walk_decl symtab = function
  | Ast.TypeBindings b ->
    walk_type_bindings symtab b
  | Ast.ValueBindings b ->
    let () = print_endline "# Walk value declaration" in
    let symtab, _ = walk_value_bindings symtab empty_idmap b in
    symtab
  | Ast.MethodBindings b ->
    walk_method_bindings symtab b

let walk_decl_list l =
  let symtab = default_symtab in
  List.fold_left walk_decl symtab l