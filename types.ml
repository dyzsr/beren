open Utils

let {new_id=new_tyvar_id} = make_idgen 0
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
  | Tyvar of tyvar
  | Tuple of datatype list
  | Variants of id * variant list (* variants *)
  | Record of id * field list (* fields *)
  | Interface of signature list
  | Function of datatype * datatype
  | Generics of id * tyvar list * datatype
  | Specific of datatype list * datatype
  | Alias of string * datatype (* a complete type with an alias *)
  | Pending of string (* an incomplete type with only a name *)

(* Type variable *)
and tyvar = string * id

and variant = int * string * datatype ref option
and field = int * string (* name *) * datatype ref
and signature = string * datatype ref

let new_tyvar name =
  (name, new_tyvar_id ())

module Name_map = Utils.Name_map
module Id_map = Utils.Id_map

(* let empty_string_map () = Name_map.empty *)

(* make an outer scope with a default value map *)
let make_string_map defaults : 'a Name_map.t =
  Name_map.of_seq (List.to_seq defaults)

(* Symbol table *)
type symtab =
  { types : datatype Name_map.t
  ; values : datatype Name_map.t
  ; constructors : (variant * datatype) Name_map.t
  ; fields : (field * datatype) Name_map.t
  }

exception Unbound_type of string

let lookup_type name {types=types} = 
  match Name_map.find_opt name types with
  | None -> raise (Unbound_type name)
  | Some typ -> typ

let add_type name typ ({types=types} as symtab) =
  let types = Name_map.add name typ types in
  {symtab with types=types}

exception Unbound_value of string

let lookup_value name {values=values} =
  match Name_map.find_opt name values with
  | None -> raise (Unbound_value name)
  | Some sym -> sym
  
let add_value name sym ({values=values} as symtab) =
  let values = Name_map.add name sym values in
  {symtab with values=values}
  
exception Unbound_constructor of string

let lookup_constructor name {constructors=constructors} =
  match Name_map.find_opt name constructors with
  | None -> raise (Unbound_constructor name)
  | Some con -> con

let add_constructor name con ({constructors=constructors} as symtab) =
  let constructors = Name_map.add name con constructors in
  {symtab with constructors=constructors}

exception Unbound_field of string

let lookup_field name {fields=fields} =
  match Name_map.find_opt name fields with
  | None -> raise (Unbound_field name)
  | Some field -> field

let add_field name field ({fields=fields} as symtab) =
  let fields = Name_map.add name field fields in
  {symtab with fields=fields}

(* Type variables table *)
type tvtab =
  { tvmap : id Name_map.t (* name -> datatype *)
  ; idmap : datatype Id_map.t (* id -> datatype *)
  }

let empty_tvtab =
  { tvmap = Name_map.empty
  ; idmap = Id_map.empty
  }

exception Unbound_type_variable of string
exception Duplicate_type_variable of string

let bind_tyvar name key ({tvmap} as tvtab) =
  let update = function
  | None -> Some key
  | Some id when id = key -> Some key
  | Some _ -> raise (Duplicate_type_variable name)
  in
  let tvmap = Name_map.update name update tvmap in
  { tvtab with tvmap }

let lookup_tyvar name {tvmap} =
  match Name_map.find_opt name tvmap with
  | None -> raise (Unbound_type_variable name)
  | Some tyvar -> tyvar

let lookup_tyvar_opt name {tvmap} =
  Name_map.find_opt name tvmap

let next_free_tyvar key tvtab =
  let rec iter ch count =
    let name =
      if count = 0 then String.make 1 ch
      else String.make 1 ch ^ string_of_int count
    in
    match lookup_tyvar_opt name tvtab with
    | None -> name
    | Some id when id = key -> name
    | Some _ ->
      if ch = 'z' then iter 'a' (count + 1)
      else iter (char_of_int (int_of_char ch + 1)) count
  in
  iter 'a' 0

let rec find_id key tvtab =
  (* let () = print_endline ("find_tvtab: " ^ string_of_int key) in *)
  let tyvar = Tyvar ("a", key) in
  match lookup_id key tvtab with
  | None -> (* found nothing *)
    let tvtab = add_id key tyvar tvtab in (* update *)
    tvtab, tyvar
  | Some (Tyvar (_, id) as t) when id = key -> (* found itself *)
    tvtab, t
  | Some (Tyvar (_, id)) -> (* found other type variable *)
    let tvtab, t = find_id id tvtab in (* find root *)
    let tvtab = add_id key t tvtab in (* update the path to root *)
    tvtab, t
  | Some t -> (* found a datatype other than type variable *)
    tvtab, t

and add_id key typ ({idmap} as tvtab) =
  let idmap = Id_map.add key typ idmap in
  { tvtab with idmap }

and lookup_id key {idmap} =
  Id_map.find_opt key idmap


(* Stringify *)
let rec string_of_toplevel_datatype = function
  | Alias (name, Generics (_, tyvars, typ)) ->
    let fold tvtab (name, id) =
      bind_tyvar name id tvtab
    in
    let string_of_tyvars = function
    | [(name, id)] -> ("'" ^ name)
    | l ->
      let tyvars = List.map (fun (name, _) -> "'" ^ name) l in
      "(" ^ String.concat ", " tyvars ^ ")"
    in
    let tvtab = List.fold_left fold empty_tvtab tyvars in
    let tyvars_string = string_of_tyvars tyvars in
    (match typ with
     | Ref -> "type " ^ tyvars_string ^ " ref"
     | Array -> "type " ^ tyvars_string ^ " array"
     | _ ->
       let _, str = string_of_datatype tvtab 0 typ in
       "type " ^ tyvars_string ^ " " ^ name ^ " = " ^ str)
  | Alias (name, typ) ->
    let _, str = string_of_datatype empty_tvtab 0 typ in
    "type " ^ name ^ " = " ^ str
  | t ->
    let _, str = string_of_datatype empty_tvtab 0 t in
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
  | Tyvar (name, key) ->
    let tvtab, name =
      match lookup_tyvar_opt name tvtab with
      | None ->
        let tvtab = bind_tyvar name key tvtab in
        tvtab, name
      | Some _ ->
        let name = next_free_tyvar key tvtab in
        let tvtab = bind_tyvar name key tvtab in
        tvtab, name
    in
    tvtab, "'(" ^ name ^ ", " ^ string_of_int key ^ ")"
    (* tvtab, "'" ^ name *)
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
      | (_, name, None) -> tvtab, name
      | (_, name, Some typ) ->
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
    | (_, name, typ) :: l ->
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
  | Generics (id, params, body) ->
    tvtab, ("generics: " ^ string_of_int id)
  | Specific (args, body) ->
    let rec iter tvtab acc = function
    | [] -> tvtab, acc
    | arg :: l ->
      let tvtab, arg = string_of_datatype tvtab 30 arg in
      iter tvtab (arg :: acc) l
    in
    let tvtab, args = iter tvtab [] args in
    let args =
      (match args with
      | [x] -> x
      | _ -> "(" ^ String.concat ", " (List.rev args) ^ ")"
      )
    in
    let tvtab, body = string_of_datatype tvtab 0 body in
    tvtab, args ^ " " ^ body
  | Alias (name, _) -> tvtab, name
  | Pending name -> tvtab, "[" ^ name ^ "]"

let get_type_string typ =
  let _, str = string_of_datatype empty_tvtab 0 typ in str

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
  Name_map.iter print_type symtab.types;
  Name_map.iter print_value symtab.values;
  ()

(* Update pending types in tuple, function, and generics *)
let rec update_pending symtab = function
  | Unit | Bool | Int | Char | String | Ref | Array as t -> t
  | Tyvar _ as t -> t
  | Tuple l -> Tuple (List.map (update_pending symtab) l)
  | Variants _ as t -> t
  | Record _ as t -> t
  | Interface _ as t -> t
  | Function (arg, body) -> Function (update_pending symtab arg, update_pending symtab body)
  | Generics (id, params, body) -> Generics (id, params, update_pending symtab body)
  | Specific (args, body) ->
    let args = List.map (update_pending symtab) args in
    Specific (args, update_pending symtab body)
  | Alias _ as t -> t
  | Pending name -> lookup_type name symtab

(* Resolve pending types inside variants, record, and interface *)
let rec resolve_pending symtab = function
  | Unit | Bool | Int | Char | String | Ref | Array | Tyvar _ | Alias _ -> ()
  | Tuple l -> List.iter (resolve_pending symtab) l
  | Variants (_, l) ->
    let resolve_variant : variant -> unit = function
    | (_, _, None) -> ()
    | (_, _, Some typ) -> typ := update_pending symtab (!typ)
    in
    List.iter resolve_variant l
  | Record (_, l) ->
    let resolve_field (_, _, typ : field) =
      typ := update_pending symtab (!typ)
    in
    List.iter resolve_field l
  | Interface l ->
    let resolve_signature (_, typ : signature) =
      typ := update_pending symtab (!typ)
    in
    List.iter resolve_signature l
  | Function (arg, body) -> resolve_pending symtab arg; resolve_pending symtab body
  | Generics (_, _, body) -> resolve_pending symtab body
  | Specific (args, body) -> List.iter (resolve_pending symtab) args; resolve_pending symtab body
  | Pending _ -> failwith "resolve_pending"

(* Filter types with pending inside *)
let rec filter_pending = function
  | Unit | Bool | Int | Char | String | Ref | Array | Tyvar _ -> false
  | Tuple l ->
    let rec iter = function
    | [] -> false
    | h :: t -> if filter_pending h then true else iter t
    in
    iter l
  | Variants _ | Record _ | Interface _ -> false
  | Function (arg, body) -> filter_pending arg || filter_pending body
  | Generics (_, _, body) -> filter_pending body
  | Specific (args, body) ->
    let fold acc arg = acc || filter_pending arg in
    let args = List.fold_left fold false args in
    args || filter_pending body
  | Alias (_, typ) -> false
  | Pending _ -> true


let rec extract_alias = function
  | Alias (_, typ) -> typ
  | t -> t

let rec extract_generics_id (id, params, typ) =
  (* TODO: wrong here !! *)
  match typ with
  | Specific (_, Alias (_, Generics (inner_id, vars, _)))
      when List.length params = List.length vars -> inner_id
  | _ -> id

let rec extract_param_type = function
  | Specific (typs, _) -> typs
  | _ -> failwith "extract_param_type"

exception Type_arity_mismatch of string
exception Not_generic_type of string

let rec check_datatype = function
  | Unit | Bool | Int | Char | String -> ()
  | Ref -> failwith "check_datatype: ref"
  | Array -> failwith "check_datatype: array"
  | Tyvar _ -> ()
  | Tuple l -> List.iter check_datatype l
  | Variants (_, l) -> let aux = function
    | (_, _, None) -> ()
    | (_, _, Some t) -> check_datatype (!t)
    in
    List.iter aux l
  | Record (_, l) -> List.iter (fun (_, _, t) -> check_datatype (!t)) l
  | Interface l -> List.iter (fun (_, t) -> check_datatype (!t)) l
  | Function (arg, body) -> check_datatype arg; check_datatype body
  | Generics (_, _, Ref) | Generics (_, _, Array) -> ()
  | Generics (_, _, body) -> check_datatype body
  | Specific (args, body) -> List.iter check_datatype args;
    (match body with
    | Alias (name, Generics (_, tyvars, _)) ->
      if List.length args <> List.length tyvars then
        raise (Type_arity_mismatch name)
    | Alias (name, _) ->
      raise (Not_generic_type name)
    | t ->
      raise (Not_generic_type (get_type_string t))
    )
  | Alias (_, Alias (name, Generics _)) -> raise (Type_arity_mismatch name)
  | Alias _ -> ()
  | Pending _ -> failwith "check_datatype: pending"


(* Default type table for access *)

(* define list *)
let list_type =
  let tyvar_for_list = new_tyvar "a" in
  let ref_to_cons =
    let tyvar = Tyvar tyvar_for_list in
    ref (Tuple [tyvar; Specific ([tyvar], Pending "list")])
  in
  let list_nil = (0, "[]", None) in
  let list_cons = (1, "(::)", Some ref_to_cons) in
  let variants = Variants (new_tyvar_id (), [list_nil; list_cons]) in
  let alias = Alias ("list", Generics (new_tyvar_id (), [tyvar_for_list], variants)) in
  let tyvar = Tyvar tyvar_for_list in
  ref_to_cons := Tuple [tyvar; Specific ([tyvar], alias)];
  alias

let list_nil, list_cons =
  match list_type with
  | Alias ("list", Generics (_, _, Variants (_, [nil; cons]))) -> nil, cons
  | _ -> failwith "list_type"

(* define ref *)
let ref_type = Alias ("ref", Generics (new_tyvar_id (), [new_tyvar "a"], Ref))

(* define array *)
let array_type = Alias ("array", Generics (new_tyvar_id (), [new_tyvar "a"], Array))

(* type table *)
let default_symtab =
  (* save default types *)
  let default_type_list =
    [ "unit", Alias ("unit", Unit)
    ; "bool", Alias ("bool", Bool)
    ; "int", Alias ("int", Int)
    ; "char", Alias ("char", Char)
    ; "string", Alias ("string", String)
    ; "list", list_type
    ; "ref", ref_type
    ; "array", array_type
    ]
  in
  let default_types = make_string_map default_type_list in
  (* let print (_, typ) = print_endline (string_of_toplevel_datatype typ) in *)
  (* let () = List.iter print default_type_list in *)
  (* save default values *)
  let default_value_list =
    let tyvar = Tyvar (new_tyvar "a") in
    [ "ref", Function (tyvar, Specific ([tyvar], ref_type))
    ; "print_int", Function (Int, Unit)
    ; "print_endline", Function (String, Unit)
    ]
  in
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
  ; fields = Name_map.empty
  }


let rec apply_real_type tvtab = function
  | Unit | Bool | Int | Char | String | Ref | Array as t -> t
  | Tyvar (_, key) ->
    let tvtab, t = find_id key tvtab in
    (match t with
    | Tyvar (_, id) when id = key -> t
    | _ -> apply_real_type tvtab t
    )
  | Tuple l ->
    Tuple (List.map (apply_real_type tvtab) l)
  | Variants _ | Record _ | Interface _ as t -> t
  | Function (arg, body) ->
    Function (apply_real_type tvtab arg, apply_real_type tvtab body)
  (* | Generics _ as t -> t *)
  | Specific (args, body) ->
    let args = List.map (apply_real_type tvtab) args in
    Specific (args, body)
  | Alias (name, typ) ->
    Alias (name, apply_real_type tvtab typ)
  | t ->
    let _, msg = string_of_datatype empty_tvtab 0 t in
    failwith ("apply_real_type: " ^ msg)

let rec apply_generics tvtab = function
  | Unit | Bool | Int | Char | String | Ref | Array as t -> t
  | Tyvar (_, key) as typ ->
    (match lookup_id key tvtab with
    | Some t -> t
    | None -> typ
    )
  | Tuple l ->
    Tuple (List.map (apply_generics tvtab) l)
  | Variants _ | Record _ | Interface _ as t -> t
  | Function (arg, body) ->
    Function (apply_generics tvtab arg, apply_generics tvtab body)
  | Specific (args, Alias (_, Generics (_, tyvars, body))) as t ->
    (match body with
    | Variants _ | Record _ | Interface _ -> t
    | _ ->
      let args = List.map (apply_generics tvtab) args in
      let fold tvtab (_, key) arg = add_id key arg tvtab in
      let tvtab = List.fold_left2 fold tvtab tyvars args in
      apply_generics tvtab body
    )
  | Alias (_, typ) -> apply_real_type tvtab typ
  | t ->
    let _, msg = string_of_datatype empty_tvtab 0 t in
    failwith ("apply_generics: " ^ msg)