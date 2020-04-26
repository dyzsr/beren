%{
  open Utils
  open Vm
%}

%token EOF EOL
%token <string> ID
%token <string> LABEL
%token ENTRY GLOBAL EXTERNAL
%token CAPTURED NUMVARS FUNC BODY

%token LPAR "(" RPAR ")" COMMA ","
%token WILDCARD "_"

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
    t=ID "(" v=nameid ")" { make_var v t }
  | "_" { Wildcard }

functions:
    { [||] }
  | l=func_list { Array.of_list l }

func_list:
    f=func { [f] }
  | f=func l=func_list { f :: l }

func:
    FUNC id=nameid eol
    captured=captured_vars
    n=num_vars eol
    BODY eol proc=procedure
    {
      let func = {id; captured; nvars=n; proc} in
      link_labels func; func
    }

captured_vars:
    { [||] }
  | CAPTURED eol l=nameid_list { Array.of_list l }

num_vars:
    NUMVARS n=INT { n }

entry_point:
    ENTRY eol
    n=num_vars eol
    BODY eol proc=procedure
    {{id=(-1, ""); captured=[||]; nvars=n; proc=proc}}

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
    op=ID nameid=nameid var=var { D (cmd_decl op, nameid, var) }

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
