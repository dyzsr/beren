open Utils

(* Variables *)
type nameid = int * string

let text_nameid (id, name) =
  string_of_int id ^ ", " ^ name

type var =
  | External of nameid
  | Global of nameid
  | Outer of nameid
  | Local of nameid
  | Wildcard

exception Invalid_var_type of string
let make_var (n, name) = function
  | "external" -> External (n, name)
  | "global" -> Global (n, name)
  | "outer" -> Outer (n, name)
  | "local" -> Local (n, name)
  | _ -> raise (Invalid_var_type name)

let text_var = function
  | External (id, name) -> "external(" ^ text_nameid (id, name) ^ ")"
  | Global (id, name) -> "global(" ^ text_nameid (id, name) ^ ")"
  | Outer (id, name) -> "outer(" ^ text_nameid (id, name) ^ ")"
  | Local (id, name) -> "local(" ^ text_nameid (id, name) ^ ")"
  | Wildcard -> "_"

(* Instruction set *)

let get_nameid m name =
  Name_map.find name m

let to_map l =
  Name_map.of_seq (List.to_seq l)

type label = string

type op_single =
  | Ret
  | Nop
  | Crash

let cmd_single =
  let mapping =
    [ "ret", Ret
    ; "nop", Nop
    ; "crash", Crash
    ] in
  get_nameid (to_map mapping)

let text_single = function
  | Ret -> "ret"
  | Nop -> "nop"
  | Crash -> "crash"

type op_jmp =
  | JmpTrue | JmpFalse | Jmp

let cmd_jmp =
  let mapping =
    [ "jmptrue", JmpTrue
    ; "jmpfalse", JmpFalse
    ; "jmp", Jmp
    ] in
  get_nameid (to_map mapping)

let text_jmp = function
  | JmpTrue -> "jmptrue"
  | JmpFalse -> "jmpfalse"
  | Jmp -> "jmp"

type op_decl =
  | MakeFunc

let cmd_decl =
  let mapping = 
    [ "makefunc", MakeFunc
    ] in
  get_nameid (to_map mapping)

let text_decl = function
  | MakeFunc -> "makefunc"

type op_v =
  | LoadUnit
  | SetArg | GetRet | GetArg | SetRet | Call
  | SetVal | PushPart | PushField
  | MakeTuple | MakeVariant | MakeRecord

let cmd_v =
  let mapping =
    [ "loadunit", LoadUnit
    ; "setarg", SetArg
    ; "getret", GetRet
    ; "getarg", GetArg
    ; "setret", SetRet
    ; "call", Call
    ; "setval", SetVal
    ; "pushpart", PushPart
    ; "pushfield", PushField
    ; "maketuple", MakeTuple
    ; "makevariant", MakeVariant
    ; "makerecord", MakeRecord
    ] in
  get_nameid (to_map mapping)

let text_v = function
  | LoadUnit -> "loadunit"
  | SetArg ->  "setarg" 
  | GetRet ->  "getret" 
  | GetArg ->  "getarg" 
  | SetRet ->  "setret" 
  | Call -> "call"
  | SetVal -> "setval"
  | PushPart -> "pushpart"
  | PushField -> "pushfield"
  | MakeTuple -> "maketuple"
  | MakeVariant -> "makevariant"
  | MakeRecord -> "makerecord"

type op_i =
  | SetNum

let cmd_i =
  let mapping =
    [ "setnum", SetNum
    ] in
  get_nameid (to_map mapping)

let text_i = function
  | SetNum -> "setnum"

type op_vb = LoadBool

let cmd_vb =
  let mapping =
    [ "loadbool", LoadBool
    ] in
  get_nameid (to_map mapping)

let text_vb = function
  | LoadBool -> "loadbool"

type op_vi =
  | LoadInt

let cmd_vi =
  let mapping =
    [ "loadint", LoadInt
    ] in
  get_nameid (to_map mapping)

let text_vi = function
  | LoadInt -> "loadint"

type op_vc = LoadChar

let cmd_vc =
  let mapping =
    [ "loadchar", LoadChar
    ] in
  get_nameid (to_map mapping)

let text_vc = function
  | LoadChar -> "loadchar"

type op_vs = LoadStr

let cmd_vs =
  let mapping =
    [ "loadstr", LoadStr
    ] in
  get_nameid (to_map mapping)

let text_vs = function
  | LoadStr -> "loadstr"

type op_vv =
  | Move | SetRef | Deref
  | VariantNum | VariantVal
  | IntNeg
  | StrLen
  | BoolNot

let cmd_vv =
  let mapping =
    [ "move", Move
    ; "setref", SetRef
    ; "deref", Deref
    ; "vrnum", VariantNum
    ; "vrval", VariantVal
    ; "intneg", IntNeg
    ; "strlen", StrLen
    ; "boolnot", BoolNot
    ] in
  get_nameid (to_map mapping)

let text_vv = function
  | Move -> "move"
  | SetRef -> "setref"
  | Deref -> "deref"
  | VariantNum -> "vrnum"
  | VariantVal -> "vrval"
  | IntNeg -> "intneg"
  | StrLen -> "strlen"
  | BoolNot -> "boolnot"

type op_vvv =
  | IntAdd | IntSub | IntMul | IntDiv | IntMod
  | BoolAnd | BoolOr
  | Lt | Lte | Gt | Gte | Eq | Neq
  | StrCat | StrGet
  | Select

let cmd_vvv =
  let mapping = 
    [ "intadd", IntAdd
    ; "intsub", IntSub
    ; "intmul", IntMul
    ; "intdiv", IntDiv
    ; "intmod", IntMod
    ; "booland", BoolAnd
    ; "boolor", BoolOr
    ; "lt", Lt
    ; "lte", Lte
    ; "gt", Gt
    ; "gte", Gte
    ; "eq", Eq
    ; "neq", Neq
    ; "strcat", StrCat
    ; "strget", StrGet
    ; "select", Select
    ] in
  get_nameid (to_map mapping)

let text_vvv = function
  | IntAdd -> "intadd"
  | IntSub -> "intsub"
  | IntMul -> "intmul"
  | IntDiv -> "intdiv"
  | IntMod -> "intmod"
  | BoolAnd -> "booland"
  | BoolOr -> "boolor"
  | Lt -> "lt"
  | Lte -> "lte"
  | Gt -> "gt"
  | Gte -> "gte"
  | Eq -> "eq"
  | Neq -> "neq"
  | StrCat -> "strcat"
  | StrGet -> "strget"
  | Select -> "select"

type op_vvi =
  | TuplePart | RecordField | Capture

let cmd_vvi =
  let mapping =
    [ "tuplepart", TuplePart
    ; "recordfield", RecordField
    ; "capture", Capture
    ] in
  get_nameid (to_map mapping)

let text_vvi = function
  | TuplePart -> "tuplepart"
  | RecordField -> "recordpart"
  | Capture -> "capture" 

(* SSA form *)

type pos = ssa list
and proc = ssa list

and ssa =
  | L of label
  | S of op_single
  | J of op_jmp * target
  | D of op_decl * nameid * var
  | V of op_v * var
  | I of op_i * int
  | VB of op_vb * var * bool
  | VI of op_vi * var * int
  | VC of op_vc * var * char
  | VS of op_vs * var * string
  | VV of op_vv * var * var
  | VVV of op_vvv * var * var * var
  | VVI of op_vvi * var * var * int

and target =
  { label : label
  ; mutable pos : pos
  }

let make_target label =
  { label = label
  ; pos = []
  }

let make_l label = L label
let make_s op_name = S op_name

let text_ssa = function
  | L l -> l
  | S op ->
    "  " ^ text_single op
  | J (op, target) ->
    "  " ^ text_jmp op ^ " " ^ target.label
  | D (op, nameid, v) ->
    "  " ^ text_decl op ^ " " ^ text_nameid nameid ^ " " ^ text_var v
  | V (op, v) ->
    "  " ^ text_v op ^ " " ^ text_var v
  | I (op, i) ->
    "  " ^ text_i op ^ " " ^ string_of_int i
  | VB (op, v, b) ->
    "  " ^ text_vb op ^ " " ^ text_var v ^ " " ^ string_of_bool b
  | VI (op, v, i) ->
    "  " ^ text_vi op ^ " " ^ text_var v ^ " " ^ string_of_int i
  | VC (op, v, c) ->
    "  " ^ text_vc op ^ " " ^ text_var v ^ " #'" ^ String.make 1 c ^ "'"
  | VS (op, v, s) ->
    "  " ^ text_vs op ^ " " ^ text_var v ^ " \"" ^ s ^ "\""
  | VV (op, va, vb) ->
    "  " ^ text_vv op ^ " " ^ text_var va ^ " " ^ text_var vb
  | VVV (op, dest, va, vb) ->
    "  " ^text_vvv op ^ " " ^ text_var dest ^ " " ^ text_var va ^ " " ^ text_var vb
  | VVI (op, dest, v, i) ->
    "  " ^ text_vvi op ^ " " ^ text_var dest ^ " " ^ text_var v ^ " " ^ string_of_int i

let text_proc l =
  let texts = List.map text_ssa l in
  String.concat "\n" texts ^ "\n"

(* Intermediate represetations *)
type func =
  { id : nameid
  ; captured : nameid array
  (* ; prototype : unit *)
  ; nvars : int
  ; proc : proc
  }

let text_func func =
  let nameid = ".func " ^ text_nameid func.id ^ "\n" in
  let captured = match Array.length func.captured with
    | 0 -> ""
    | _ ->
      let indent x = "  " ^ text_nameid x ^ "\n" in
      let capts = Array.map indent func.captured in
      let text = Array.fold_left (^) "" capts in
      ".captured\n" ^ text
  in
  let nvars = ".numvars " ^ string_of_int func.nvars ^ "\n" in
  let proc = ".body\n" ^ text_proc func.proc ^ "\n" in
  nameid ^ captured ^ nvars ^ proc

type program =
  { exts : nameid array
  ; globs : nameid array
  ; funcs : func array
  ; entry : func
  }

let text_program program =
  let indent x = "  " ^ text_nameid x ^ "\n" in
  let exts =
    if Array.length program.exts = 0 then "" else
    let text = Array.fold_left (^) "" (Array.map indent program.exts) in
    ".external\n" ^ text ^ "\n"
  in
  let globs =
    if Array.length program.globs = 0 then "" else
    let text = Array.fold_left (^) "" (Array.map indent program.globs) in
    ".global\n" ^ text ^ "\n"
  in
  let funcs = Array.fold_left (^) "" (Array.map text_func program.funcs) in
  let entry =
    let nvars = ".numvars " ^ string_of_int program.entry.nvars ^ "\n" in
    let proc = ".body\n" ^ text_proc program.entry.proc ^ "\n" in
    ".entry\n" ^ nvars ^ proc ^ "\n"
  in
  exts ^ globs ^ funcs ^ entry


exception Duplicate_label of string
let add_label m label pos =
  let f = function
  | None -> Some pos
  | Some _ -> raise (Duplicate_label label)
  in
  Name_map.update label f m

exception Undefined_label of string
let resolve_target m ({label} as target) =
  match Name_map.find_opt label m with
  | None -> raise (Undefined_label label)
  | Some pos -> target.pos <- pos

let link_labels func =
  let rec collect m = function
  | [] -> m
  | ssa :: l as pos ->
    let m = match ssa with
    | L label -> add_label m label pos
    | _ -> m
    in
    collect m l
  in
  let map = collect Name_map.empty func.proc in
  let rec resolve = function
  | [] -> ()
  | ssa :: l ->
    let () = match ssa with
    | J (_, target) -> resolve_target map target
    | _ -> ()
    in
    resolve l
  in
  resolve func.proc

(* Runtime values *)
type value =
  | Nil
  | Unit
  | Bool of bool
  | Int of int
  | Char of char
  | String of string
  | Ref of value ref
  | Tuple of value array
  | Array of value array
  | Variant of int * value
  | Record of value array
  | Function of funcval
  | Builtin of nameid

and funcval = {capts: value array; func: func}

let rec string_of_value = function
  | Nil -> "nil"
  | Unit -> "()"
  | Bool b -> "bool " ^ string_of_bool b
  | Int i -> "int " ^ string_of_int i
  | Char c -> "char '" ^ String.make 1 c ^ "'"
  | String s -> "string \"" ^ s ^"\""
  | Ref {contents=v} -> "ref " ^ string_of_value v
  | Tuple l ->
    let strs = Array.map string_of_value l in
    let str = String.concat ", " (Array.to_list strs) in
    "(" ^ str ^ ")"
  | Array l -> failwith "string_of_value"
  | Variant (i, v) ->
    "variant (" ^ string_of_int i ^ ", " ^ string_of_value v ^ ")"
  | Record _ -> failwith "string_of_value"
  | Function _ -> "function"
  | Builtin (id, name) -> "builtin: " ^ name

exception Not_a_unit
let get_unit = function Unit -> () | _ -> raise Not_a_unit

exception Not_a_bool
let get_bool = function Bool b -> b | _ -> raise Not_a_bool

exception Not_an_int
let get_int = function Int i -> i | _ -> raise Not_an_int

exception Not_a_char
let get_char = function Char c -> c | _ -> raise Not_a_char

exception Not_a_string
let get_string = function String s -> s | _ -> raise Not_a_string

exception Not_a_tuple
let get_tuple = function Tuple v -> v | _ -> raise Not_a_tuple

exception Not_a_variant
let get_variant = function Variant (n, v) -> (n, v) | _ -> raise Not_a_variant

exception Not_a_record
let get_record = function Record v -> v | _ -> raise Not_a_record

exception Not_a_ref
let get_ref = function Ref v -> v | _ -> raise Not_a_ref

exception Not_a_function
let get_func = function Function f -> f | _ -> raise Not_a_function

exception Not_same_kind of string * string
let same_kind = function
  | Unit, Unit | Bool _, Bool _ | Int _, Int _ | Char _, Char _
  | String _, String _ | Ref _, Ref _ | Tuple _, Tuple _
  | Variant _, Variant _ | Record _, Record _ | Function _, Function _ -> ()
  | v1, v2 -> raise (Not_same_kind (string_of_value v1, string_of_value v2))

(* Call frame & stack *)
type frame =
  { func : func
  ; vars : value array
  ; capts : value array
  ; arg : value
  ; mutable pos : pos
  ; mutable ret : value
  }

type callstack = frame list

exception Captured_number_mismatch of int * int

let new_frame (func, arg, captured) =
  let {captured=capvars; nvars; proc=pos} = func in
  let capnum = Array.length capvars in
  let captnum = Array.length captured in
  if captnum <> capnum then
    raise (Captured_number_mismatch (captnum, capnum));
  { func = func
  ; pos = pos
  ; arg = arg
  ; ret = Nil
  ; vars = Array.make nvars Nil
  ; capts = captured
  }

(* The virtual machine *)
exception Invalid_operation

type machine =
  { program : program
  ; gvars : value array
  ; bltins : nameid array
  ; mutable state : bool
  ; mutable frame : frame
  ; mutable stack : callstack
  ; mutable pc : pos
  ; mutable cond : bool
  ; mutable tuple : value list
  ; mutable variant : int * value
  ; mutable record : value list
  ; mutable arg : value
  ; mutable ret : value
  ; mutable captured : value list
  }

let next_pos pos =
  match pos with
  | [] -> failwith "next_ssa"
  | _ :: next -> next

let next_ssa m =
  m.pc <- next_pos m.pc

let get_var m = function
  | External (id, _) -> Builtin m.bltins.(id)
  | Global (id, _) -> m.gvars.(id)
  | Local (id, _) -> m.frame.vars.(id)
  | Outer (id, _) -> m.frame.capts.(id)
  | Wildcard -> failwith "get_var"

let set_var m v = function
  | External (id, _) -> raise Invalid_operation
  | Global (id, _) -> m.gvars.(id) <- v
  | Local (id, _) -> m.frame.vars.(id) <- v
  | Outer (id, _) -> failwith "set_var: outer"
  | Wildcard -> ()

let call_func m ({capts; func} : funcval) =
  (* let () =
    let _, name = func.id in
    print_endline ("call " ^ name)
  in *)
  let frame = new_frame (func, m.arg, capts) in
  m.frame.pos <- m.pc;
  m.stack <- m.frame :: m.stack;
  m.frame <- frame;
  m.pc <- frame.func.proc

let bltin_funcs =
  [|0, "print_bool"
  ; 1, "print_int"
  ; 2, "print_char"
  ; 3, "print_string"
  ; 4, "print_endline"
  |]

let externals =
  let f (i, x) = x, (i, x) in
  Name_map.of_seq (Array.to_seq (Array.map f bltin_funcs))

let call_builtin m (id, name) =
  (match id with
  | 0 -> let b = get_bool m.arg in
    if b then print_string "true" else print_string "false"; m.ret <- Unit
  | 1 -> let i = get_int m.arg in print_int i; m.ret <- Unit
  | 2 -> let c = get_char m.arg in print_char c; m.ret <- Unit
  | 3 -> let s = get_string m.arg in print_string s; m.ret <- Unit
  | 4 -> let s = get_string m.arg in print_endline s; m.ret <- Unit
  | _ -> failwith "call_builtin"
  );
  next_ssa m

(* Run the machine *)
let create program =
  let globs = program.globs in
  let gvars = Array.make (Array.length globs) Nil in
  let entry = program.entry in
  let frame = new_frame (entry, Nil, [||]) in
  { program = program
  ; gvars = gvars
  ; bltins = bltin_funcs
  ; state = true
  ; frame = frame
  ; stack = []
  ; pc = frame.pos
  ; cond = false
  ; tuple = []
  ; variant = (0, Nil)
  ; record = []
  ; arg = Nil
  ; ret = Nil
  ; captured = []
  }

let rec run (m : machine) =
  match m.pc with
  | [] -> failwith "run"
  | ssa :: rest ->
    match ssa with
    | L _ ->
      (* print_endline (text_ssa ssa); *)
      (); next_ssa m
    | S op ->
      (* print_endline (text_ssa ssa); *)
      run_s m op
    | J (op, pos) ->
      (* print_endline (text_ssa ssa); *)
      run_j m (op, pos)
    | D (op, nameid, var) ->
      (* print_endline (text_ssa ssa); *)
      run_d m (op, nameid, var)
    | V (op, var) ->
      (* print_endline (text_ssa ssa); *)
      run_v m (op, var)
    | I (op, i) ->
      (* print_endline (text_ssa ssa); *)
      run_i m (op, i)
    | VB (op, var, b) ->
      (* print_endline (text_ssa ssa); *)
      run_vb m (op, var, b)
    | VI (op, var, i) ->
      (* print_endline (text_ssa ssa); *)
      run_vi m (op, var, i)
    | VC (op, var, c) ->
      (* print_endline (text_ssa ssa); *)
      run_vc m (op, var, c)
    | VS (op, var, s) ->
      (* print_endline (text_ssa ssa); *)
      run_vs m (op, var, s)
    | VV (op, dest, src) ->
      (* print_endline (text_ssa ssa); *)
      run_vv m (op, dest, src)
    | VVV (op, dest, va, vb) ->
      (* print_endline (text_ssa ssa); *)
      run_vvv m (op, dest, va, vb)
    | VVI (op, dest, var, i) ->
      (* print_endline (text_ssa ssa); *)
      run_vvi m (op, dest, var, i)

and run_s m = function
  | Ret ->
    m.ret <- m.frame.ret;
    (match m.stack with
    | [] ->
      m.state <- false 
    | frame :: stack ->
      m.frame <- frame;
      m.stack <- stack;
      m.pc <- next_pos frame.pos
    )
  | Nop -> next_ssa m
  | Crash -> failwith "crash"

and run_j m (op, target) =
  match op with
  | JmpTrue ->
    if m.cond then m.pc <- target.pos
    else next_ssa m
  | JmpFalse ->
    if (not m.cond) then m.pc <- target.pos
    else next_ssa m
  | Jmp ->
    m.pc <- target.pos

and run_d m (op, (src_id, _), var) =
  (match op with
  | MakeFunc ->
    let func = m.program.funcs.(src_id) in
    let capts = Array.make (Array.length func.captured) Nil in
    set_var m (Function {capts; func}) var
  );
  next_ssa m

and run_v m (op, var) =
  match op with
  | LoadUnit -> set_var m Unit var; next_ssa m
  | SetArg -> m.arg <- get_var m var; next_ssa m
  | GetRet -> set_var m m.ret var; next_ssa m
  | GetArg -> set_var m m.frame.arg var; next_ssa m
  | SetRet -> m.frame.ret <- get_var m var; next_ssa m
  | Call ->
    (match get_var m var with
    | Function f -> call_func m f
    | Builtin (id, name) -> call_builtin m (id, name)
    | _ -> raise Not_a_function
    )
  | SetVal ->
    let (num, _) = m.variant in
    m.variant <- (num, get_var m var); next_ssa m
  | PushPart ->
    m.tuple <- get_var m var :: m.tuple; next_ssa m
  | PushField ->
    m.record <- get_var m var :: m.record; next_ssa m
  | MakeTuple ->
    set_var m (Tuple (Array.of_list m.tuple)) var;
    m.tuple <- []; next_ssa m
  | MakeVariant ->
    let (num, value) = m.variant in
    set_var m (Variant (num, value)) var;
    m.variant <- (0, Nil); next_ssa m
  | MakeRecord ->
    set_var m (Record (Array.of_list m.record)) var;
    m.record <- []; next_ssa m

and run_i m (op, i) =
  (match op with
  | SetNum ->
    let _, val_ = m.variant in m.variant <- (i, val_)
  );
  next_ssa m

and run_vb m (op, var, b) =
  (match op with
  | LoadBool -> set_var m (Bool b) var
  );
  next_ssa m

and run_vi m (op, var, i) =
  (match op with
  | LoadInt -> set_var m (Int i) var
  );
  next_ssa m

and run_vc m (op, var, c) =
  (match op with
  | LoadChar -> set_var m (Char c) var
  );
  next_ssa m

and run_vs m (op, var, s) =
  (match op with
  | LoadStr -> set_var m (String s) var
  );
  next_ssa m

and run_vv m (op, dest, src) =
  (match op with
  | Move -> set_var m (get_var m src) dest
  | SetRef ->
    let value = get_ref (get_var m dest) in
    value := get_var m src
  | Deref ->
    let value = get_ref (get_var m src) in
    set_var m !value dest
  | VariantNum ->
    let (num, _) = get_variant (get_var m src) in
    set_var m (Int num) dest
  | VariantVal ->
    let (_, value) = get_variant (get_var m src) in
    set_var m value dest
  | IntNeg ->
    let i = get_int (get_var m src) in
    set_var m (Int (-i)) dest
  | StrLen ->
    let s = get_string (get_var m src) in
    set_var m (Int (String.length s)) dest
  | BoolNot ->
    let b = get_bool (get_var m src) in
    set_var m (Bool (not b)) dest
  );
  next_ssa m


and run_vvv m (op, dest, va, vb) =
  let a = get_var m va in
  let b = get_var m vb in
  let x = 
    match op with
    | IntAdd -> Int (get_int a + get_int b)
    | IntSub -> Int (get_int a - get_int b)
    | IntMul -> Int (get_int a * get_int b)
    | IntDiv -> Int (get_int a / get_int b)
    | IntMod -> Int (get_int a mod get_int b)
    | BoolAnd -> Bool (get_bool a && get_bool b)
    | BoolOr -> Bool (get_bool a || get_bool b)
    | Lt -> same_kind (a, b); let b = a < b in m.cond <- b; Bool b
    | Lte -> same_kind (a, b); let b = a <= b in m.cond <- b; Bool b
    | Gt -> same_kind (a, b); let b = a > b in m.cond <- b; Bool b
    | Gte -> same_kind (a, b); let b = a >= b in m.cond <- b; Bool b
    | Eq -> same_kind (a, b); let b = a = b in m.cond <- b; Bool b
    | Neq -> same_kind (a, b); let b = a <> b in m.cond <- b; Bool b
    | StrCat -> String (get_string a ^ get_string b)
    | StrGet -> Char (get_string a).[get_int b]
    | Select ->
      (match get_var m va, get_var m vb with
      | Nil, Nil -> failwith "select: both nil"
      | Nil, v -> v
      | v, Nil -> v
      | _, _ -> failwith "select: neither nil"
      )
  in
  set_var m x dest;
  next_ssa m

and run_vvi m (op, dest, var, i) =
  (match op with
  | TuplePart ->
    let tuple = get_tuple (get_var m var) in
    set_var m tuple.(i) dest
  | RecordField ->
    let record = get_record (get_var m var) in
    set_var m record.(i) dest
  | Capture ->
    let {capts}:funcval = get_func (get_var m dest) in
    capts.(i) <- get_var m var
  );
  next_ssa m