%{
  open Ast
%}

%token LET REC AND IN
%token FUN TO /* fun ... -> ... */
%token <string> IDENT
%token <unit> UNIT
%token <int> INT
%token <bool> BOOL
%token PLUS MINUS TIMES DIV MOD
%token LT LTE GT GTE EQ NEQ
%token LOGIC_AND LOGIC_OR
%token LPAREN RPAREN

%start global_decl
%type <Ast.decl> global_decl
%type <Ast.expr> expr

%%

global_decl:
    decl = fun_decl { decl }
  | decl = val_decl { decl }
  ;

fun_decl:
    LET is_rec=is_rec id=IDENT args=arg_list EQ body=expr {
      let func = Function (Prototype (args, body)) in
      GlobalDecl (Some id, is_rec, func)
      GlobalDecl {is_rec=false; bindings=[]}
    }
  ;

is_rec:
        { false }
  | REC { true }
  ;

fun_bindings:
    binding=fun_binding {
      match bindings with
      | 
    }
  | fun_binding AND fun_bindings {}
  ;

fun_binding:
    id=IDENT args=arg_list EQ body=expr { (id, Function (Prototype args, expr)) }
  ;

arg_list:
    id=ident_opt { [id] }
  | id=ident_opt rest=arg_list { id :: rest }
  ; 

val_decl:
    LET id=ident_opt EQ body=expr { GlobalDecl (id, false, body) }
  ;

ident_opt:
    id=IDENT { Some id }
  | unit     { None }
  ;

expr:
    e=bin_op { e }
  ;

bin_expr:
    e=logic_or_op { e }
  ;

logic_or_op:
    e=logic_and_op { e }
  | e1=logic_or_op LOGIC_OR e2=logic_and_op { Binary (Or, e1, e2) }
  ;

logic_and_op:
    cmp_op {}
  | logic_and_op LOGIC_AND cmp_op {}
  ;

cmp_op:
    plus_op {}
  | cmp_op LT plus_op {}
  | cmp_op LTE plus_op {}
  | cmp_op GT plus_op {}
  | cmp_op GTE plus_op {}
  | cmp_op EQ plus_op {}
  | cmp_op NEQ plus_op {}
  ;

plus_op:
    times_op {}
  | plus_go PLUS times_op {}
  | plus_go MINUS times_go {}
  ;

times_op:
    unary_op {}
  | times_go TIMES unary_op {}
  | times_go DIV unary_op {}
  | times_go MOD unary_op {}
  ;

unary_op:
    term {}
  | PLUS unary_op {}
  | MINUS unary_op {}
  ;

term:
    IDENT {}
  | UNIT {}
  | INT {}
  | BOOL {}
  | LPAREN expr RPAREN {}
  | function {}
  ;

function:
    FUN arg_list TO expr {}
  ;

unit:
    LPAREN RPAREN { () }
  ;

%%