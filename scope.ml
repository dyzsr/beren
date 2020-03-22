module String_map = Map.Make (String)

type 'a t = 'a String_map.t

let empty () = String_map.empty

(* make an outer scope with a default value map *)
let make defaults : 'a t =
  let aux m (name, value) =
    String_map.add name value m
  in (List.fold_left aux String_map.empty defaults)

(* add symbol to the scope *)
let add name value (map : 'a t) : 'a t =
  String_map.add name value map

let update name f (map : 'a t) =
  String_map.update name f map

(* lookup a symbol from the scope *)
let lookup name (map : 'a t) =
  String_map.find_opt name map