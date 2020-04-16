%{
  open Vm

  exception Invalid_var_type of string
  let make_var (n, name) = function
    | 0 -> External (n, name)
    | 1 -> Global (n, name)
    | 2 -> Outer (n, name)
    | 3 -> Local (n, name)
    | _ -> raise (Invalid_var_type name)

  exception Duplicate_label of string
  let add_label m label pos =
    let f = function
    | None -> Some pos
    | Some _ -> raise (Duplicate_label label)
    in
    String_map.update label f m

  exception Undefined_label of string
  let resolve_target m ({label} as target) =
    match String_map.find_opt label m with
    | None -> raise (Undefined_label label)
    | Some pos -> target.pos <- pos

  let resolve_func func =
    let rec collect m = function
    | [] -> m
    | ssa :: l as pos ->
      let m = match ssa with
      | J (_, {label}) -> add_label m label pos
      | _ -> m
      in
      collect m l
    in
    let map = collect String_map.empty func.proc in
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

%}

%token EOF EOL
%token <string> ID
%token <string> LABEL
%token ENTRY GLOBAL EXTERNAL
%token CAPTURED NUMVARS FUNC

%token DLR "$" LPAR "(" RPAR ")" COMMA ","

%token <bool> BOOL
%token <int> INT
%token <char> CHAR
%token <string> STR

%start program
%type <Vm.program> program

%%

program:
    /* starting */
    gap
    exts=externals
    globs=global_vars
    funcs=functions
    entry=entry_point
    /* ending */
    EOF
    {{ exts = exts; globs = globs; funcs = funcs; entry = entry }}

gap:
    {}
  | eol {}

eol:
    EOL {}
  | EOL eol {}

externals:
    { [||] }
  | EXTERNAL eol l=nameid_list { Array.of_list l }

global_vars:
    { [||] }
  | GLOBAL eol l=nameid_list { Array.of_list l }

nameid_list:
    v=nameid eol { [v] }
  | v=nameid eol l=nameid_list { v :: l }

nameid:
    n=INT "," name=ID { (n, name) }

var:
    "$" "(" t=INT "," v=nameid ")"
    { make_var v t }

functions:
    { [||] }
  | l=func_list { Array.of_list l }

func_list:
    f=func { [f] }
  | f=func l=func_list { f :: l }

func:
    n=num_vars eol
    FUNC ID eol
    proc=procedure
    {
      let func = {captured=None; nvars=n; proc=proc} in
      resolve_func func; func
    }
  | captured=captured_vars
    n=num_vars eol
    FUNC ID eol
    proc=procedure
    {
      let func = {captured=Some captured; nvars=n; proc=proc} in
      resolve_func func; func
    }

captured_vars:
    CAPTURED eol l=nameid_list { Array.of_list l }

num_vars:
    NUMVARS n=INT { n }

entry_point:
    ENTRY eol proc=procedure {{ captured=None; nvars=0; proc=proc }}

procedure:
    l=ssa_list { l }

ssa_list:
    ssa=ssa eol { [ssa] }
  | ssa=ssa eol l=ssa_list { ssa :: l }

ssa:
    s=ssa_label { s }
  | s=ssa_single { s }
  | s=ssa_jmp { s }
  | s=ssa_decl { s }
  | s=ssa_v { s }
  | s=ssa_i { s }
  | s=ssa_vb { s }
  | s=ssa_vi { s }
  | s=ssa_vc { s }
  | s=ssa_vs { s }
  | s=ssa_vv { s }
  | s=ssa_vvv { s }
  | s=ssa_vvi { s }

ssa_label:
    lb=LABEL { L lb }

ssa_single:
    op=ID { S (cmd_single op) }

ssa_jmp:
    op=ID lb=LABEL { J (cmd_jmp op, {label=lb; pos=[]}) }

ssa_decl:
    op=ID n=INT name=ID var=var { D (cmd_decl op, n, name, var) }

ssa_v:
    op=ID var=var { V (cmd_v op, var) }

ssa_i:
    op=ID i=INT { I (cmd_i op, i) }

ssa_vb:
    op=ID var=var b=BOOL { VB (cmd_vb op, var, b) }

ssa_vi:
    op=ID var=var i=INT { VI (cmd_vi op, var, i) }

ssa_vc:
    op=ID var=var c=CHAR { VC (cmd_vc op, var, c) }

ssa_vs:
    op=ID var=var s=STR { VS (cmd_vs op, var, s) }

ssa_vv:
    op=ID va=var vb=var { VV (cmd_vv op, va, vb) }

ssa_vvv:
    op=ID dest=var va=var vb=var { VVV (cmd_vvv op, dest, va, vb) }

ssa_vvi:
    op=ID dest=var var=var i=INT { VVI (cmd_vvi op, dest, var, i) }
