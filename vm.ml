(* Variables *)
type nameid = int * string

type var =
  | External of nameid
  | Global of nameid
  | Outer of nameid
  | Local of nameid


(* Instructions *)
module String_map = Map.Make (String)

let get m name =
  String_map.find name m

let to_map l =
  String_map.of_seq (List.to_seq l)

type label = string

type op_single =
  | Ret
  | Nop

let cmd_single =
  let mapping =
    [ "ret", Ret
    ; "nop", Nop
    ] in
  get (to_map mapping)

type op_jmp =
  | JmpTrue | JmpFalse | Jmp

let cmd_jmp =
  let mapping =
    [ "jmptrue", JmpTrue
    ; "jmpfalse", JmpFalse
    ; "jmp", Jmp
    ] in
  get (to_map mapping)

type op_decl =
  | MakeFunc | MakeClosure

let cmd_decl =
  let mapping = 
    [ "makefunc", MakeFunc
    ; "makeclos", MakeClosure
    ] in
  get (to_map mapping)

type op_v =
  | LoadUnit
  | SetArg | GetRet | GetArg | SetRet | Call
  | SetVal | PushPart | PushField | Capture
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
    ; "capture", Capture
    ; "maketuple", MakeTuple
    ; "makevariant", MakeVariant
    ; "makerecord", MakeRecord
    ] in
    get (to_map mapping)

type op_i =
  | SetNum

let cmd_i =
  let mapping =
    [ "setnum", SetNum
    ] in
    get (to_map mapping)

type op_vb = LoadBool

let cmd_vb =
  let mapping =
    [ "loadbool", LoadBool
    ] in
    get (to_map mapping)

type op_vi =
  | LoadInt

let cmd_vi =
  let mapping =
    [ "loadint", LoadInt
    ] in
    get (to_map mapping)

type op_vc = LoadChar

let cmd_vc =
  let mapping =
    [ "loadchar", LoadChar
    ] in
    get (to_map mapping)

type op_vs = LoadStr

let cmd_vs =
  let mapping =
    [ "loadstr", LoadStr
    ] in
    get (to_map mapping)

type op_vv =
  | Move | Assign | Deref
  | VariantNum | VariantVal
  | StrLen
  | BoolNot

let cmd_vv =
  let mapping =
    [ "move", Move
    ; "assign", Assign
    ; "deref", Deref
    ; "variantnum", VariantNum
    ; "variantval", VariantVal
    ; "strlen", StrLen
    ; "boolnot", BoolNot
    ] in
    get (to_map mapping)

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
  get (to_map mapping)

type op_vvi =
  | TuplePart | RecordField

let cmd_vvi =
  let mapping =
    [ "tuplepart", TuplePart
    ; "recordfield", RecordField
    ] in
  get (to_map mapping)

(* SSA form *)

type pos = ssa list
and proc = ssa list

and ssa =
  | L of label
  | S of op_single
  | J of op_jmp * target
  | D of op_decl * int * string * var
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

let make_l label = L label
let make_s op_name = S op_name

(* Intermediate represetations *)

type captured = nameid array

type func =
  { captured : captured option
  (* ; prototype : unit *)
  ; nvars : int
  ; proc : proc
  }

type program =
  { exts : nameid array
  ; globs : nameid array
  ; funcs : func array
  ; entry : func
  }

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
  | Function of func
  | Closure of value array * func
  | Builtin of nameid

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
let get_variant = function Variant (num, value) -> (num, value) | _ -> raise Not_a_variant

exception Not_a_record
let get_record = function Record v -> v | _ -> raise Not_a_record

exception Not_a_ref
let get_ref = function Ref v -> v | _ -> raise Not_a_ref

exception Not_a_function

exception Not_same_kind
let same_kind = function
  | Unit, Unit | Bool _, Bool _ | Int _, Int _ | Char _, Char _ | String _, String _
  | Ref _, Ref _ | Tuple _, Tuple _ | Variant _, Variant _ | Record _, Record _
  | Function _, Function _ | Closure _, Closure _ -> ()
  | _, _ -> raise Not_same_kind

(* Call frame & stack *)
type frame =
  { func : func
  ; vars : value array
  ; captured : value array
  ; arg : value
  ; mutable pos : pos
  ; mutable ret : value
  }

type callstack = frame list

exception Captured_number_mismatch

let new_frame func arg captured =
  let {captured=capvars; nvars; proc=pos} = func in
  let capnum = match capvars with
  | None -> 0
  | Some x -> Array.length x
  in
  if Array.length captured <> capnum then
    raise Captured_number_mismatch;
  { func = func
  ; pos = pos
  ; arg = arg
  ; ret = Nil
  ; vars = Array.make nvars Nil
  ; captured = captured
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
  | Outer (id, _) -> m.frame.captured.(id)
  | Local (id, _) -> m.frame.vars.(id)

let set_var m v = function
  | External (id, _) -> raise Invalid_operation
  | Global (id, _) -> m.gvars.(id) <- v
  | Outer (id, _) -> m.frame.captured.(id) <- v
  | Local (id, _) -> m.frame.vars.(id) <- v

let call_func m func =
  let frame = new_frame func m.arg [||] in
  m.frame.pos <- m.pc;
  m.stack <- m.frame :: m.stack;
  m.frame <- frame;
  m.pc <- frame.func.proc

let call_closure m (captured, func) =
  let frame = new_frame func m.arg captured in
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
  let frame = new_frame entry Nil [||] in
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
    | L _ -> (); next_ssa m
    | S op -> run_s m op
    | J (op, pos) -> run_j m (op, pos)
    | D (op, id, s, var) -> run_d m (op, id, s, var)
    | V (op, var) -> run_v m (op, var)
    | I (op, i) -> run_i m (op, i)
    | VB (op, var, b) -> run_vb m (op, var, b)
    | VI (op, var, i) -> run_vi m (op, var, i)
    | VC (op, var, c) -> run_vc m (op, var, c)
    | VS (op, var, s) -> run_vs m (op, var, s)
    | VV (op, va, vb) -> run_vv m (op, va, vb)
    | VVV (op, dest, va, vb) -> run_vvv m (op, dest, va, vb)
    | VVI (op, dest, var, i) -> run_vvi m (op, dest, var, i)

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

and run_d m (op, src_id, _, var) =
  (match op with
  | MakeFunc ->
    set_var m (Function m.program.funcs.(src_id)) var
  | MakeClosure ->
    let captured = Array.of_list m.captured in
    set_var m (Closure (captured, m.program.funcs.(src_id))) var
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
    | Function func -> call_func m func
    | Closure (captured, func) -> call_closure m (captured, func)
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
  | Capture ->
    m.captured <- get_var m var :: m.captured; next_ssa m
  | MakeTuple ->
    set_var m (Tuple (Array.of_list m.tuple)) var; next_ssa m
  | MakeVariant ->
    let (num, value) = m.variant in
    set_var m (Variant (num, value)) var; next_ssa m
  | MakeRecord ->
    set_var m (Record (Array.of_list m.record)) var; next_ssa m

and run_i m (op, i) =
  (match op with
  | SetNum ->
    let (_, value) = m.variant in
    m.variant <- (i, value)
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

and run_vv m (op, va, vb) =
  (match op with
  | Move -> set_var m (get_var m vb) va
  | Assign ->
    let value = get_ref (get_var m va) in
    value := get_var m vb
  | Deref ->
    let value = get_ref (get_var m vb) in
    set_var m !value va
  | VariantNum ->
    let (num, _) = get_variant (get_var m vb) in
    set_var m (Int num) va
  | VariantVal ->
    let (_, value) = get_variant (get_var m vb) in
    set_var m value vb
  | StrLen ->
    let s = get_string (get_var m vb) in
    set_var m (Int (String.length s)) va
  | BoolNot ->
    let b = get_bool (get_var m vb) in
    set_var m (Bool (not b)) va
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
    | Lt -> same_kind (a, b); let b = get_int a < get_int b in m.cond <- b; Bool b
    | Lte -> same_kind (a, b); let b = (get_int a <= get_int b) in m.cond <- b; Bool b
    | Gt -> same_kind (a, b); let b = (get_int a > get_int b) in m.cond <- b; Bool b
    | Gte -> same_kind (a, b); let b = (get_int a >= get_int b) in m.cond <- b; Bool b
    | Eq -> same_kind (a, b); let b = (get_int a = get_int b) in m.cond <- b; Bool b
    | Neq -> same_kind (a, b); let b = (get_int a <> get_int b) in m.cond <- b; Bool b
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
  );
  next_ssa m