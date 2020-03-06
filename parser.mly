%{

open Ast

(* val_of_fun: convert function definition into value bindings *)
let val_of_fun expr (Prototype (patterns, type_tag)) =
  let rec aux = function
    | [] -> failwith "val_of_fun: no enough arguments"
    | arg :: [] -> begin
      match type_tag with
        | None -> LambdaExpr [(arg, expr)]
        | Some t -> LambdaExpr [(arg, ExprWithType (expr, t))]
      end
    | arg :: rest -> LambdaExpr [(arg, aux rest)]
  in aux patterns

%}

%token EOF
%token LET "let" TYPE "type" METHOD "method" AND "and"
%token REC "rec" IN "in" MUTABLE "mutable" OF "of"
%token FUN "fun" FUNCTION "function"
%token IF "if" THEN "then" ELSE "else"
%token MATCH "match" WITH "with"
%token INTERFACE "interface" END "end"

%token WILDCARD "_"
%token <int> INT
%token <bool> BOOL
%token <char> CHAR
%token <string> STRING
%token <string> IDENT
%token <string> CAPID
%token <string> TYPESYMBOL

%token PLUS "+" MINUS "-" TIMES "*" DIV "/" MOD "%" CONCAT "^"
%token LT "<" LTE "<=" GT ">" GTE ">=" EQ "=" NEQ "!="
%token LOGIC_AND "&&" LOGIC_OR "||"
%token LPAREN "(" RPAREN ")" LBRACK "[" RBRACK "]" LBRACE "{" RBRACE "}"
%token LBRACKBAR "[|" RBRACKBAR "|]"
%token BAR "|" APPEND "@" CONS "::" DEREF "!"
%token TO "=>" ARROW "->"
%token SEMICOLON ";" COLON ":" COMMA "," PERIOD "."
%token ASSIGN ":=" ASSIGNFIELD "<-"

%start program
%type <Ast.decl list> program
%type <Ast.decl> global_decl
%type <Ast.expr> expr

%nonassoc LAST_BRANCH
%nonassoc BAR

%nonassoc THEN
%nonassoc ELSE

%%

program:
    l=global_decl_list EOF { l }

global_decl_list:
    g=global_decl { [g] }
  | g=global_decl l=global_decl_list { g :: l }

global_decl:
    decl = let_decl { ValueBinding decl }
  | decl = method_decl { MethodBinding decl }
  | decl = type_decl { TypeBinding decl }

let_decl:
    "let" r=is_rec b=let_binding
    { (r, [b]) }
  | "let" r=is_rec b=let_binding "and" l=let_binding_list
    { (r, b::l) }
  ;

is_rec:
    { false }
  | "rec" { true }
  ;

let_binding_list:
    b=let_binding { [b] }
  | b=let_binding "and" l=let_binding_list { b :: l }

let_binding:
    b=val_binding { b }
  | b=fun_binding { b }

val_binding:
    p=pattern "=" e=expr { (p, e) }

fun_binding:
    id=IDENT proto=prototype "=" e=expr
      { (VariablePattern id, val_of_fun e proto) }

method_decl:
    "method" r=receiver b=fun_binding
      { (r, (true, [b])) }
  | "method" r=receiver b=fun_binding "and" l=method_binding_list
      { (r, (true, b::l)) }

receiver:
    p=pattern_with_type { p }

method_binding_list:
    b=fun_binding { [b] }
  | b=fun_binding "and" l=method_binding_list { b :: l }

prototype:
    l=arg_list { Prototype (l, None) }
  | l=arg_list ":" t=type_expr { Prototype (l, Some t) }

arg_list:
    p=highest_prec_pattern { [p] }
  | p=highest_prec_pattern l=arg_list { p :: l }

type_decl:
    "type" b=type_binding
    { (true, [b]) }
  | "type" b=type_binding "and" l=type_binding_list
    { (true, b::l) }

type_binding_list:
    b=type_binding { [b] }
  | b=type_binding "and" l=type_binding_list { b :: l }

type_binding:
    p=type_param id=IDENT "=" t=type_construct { (p, id, t) }

type_param:
    t=type_symbol { [t] }
  | "(" t=type_symbol "," l=type_symbol_list ")" { t :: l }

type_symbol_list:
    t=type_symbol { [t] }
  | t=type_symbol "," l=type_symbol_list { t :: l }

type_symbol:
    s=TYPESYMBOL { TypeSymbol s }

type_construct:
    t=type_expr { TypeExpr t }
  | t=variant_type { t }
  | t=record_type { t }
  | t=interface_type { t }

variant_type:
    l=variant_list { VariantType l }
  | "|" l=variant_list { VariantType l }

variant_list:
    t=variant { [t] }
  | t=variant "|" l=variant_list { t :: l }

variant:
    cid=CAPID { (cid, None) }
  | cid=CAPID "of" t=type_expr { (cid, Some t) }

record_type:
    "{" "}" { RecordType [] }
  | "{" l=field_type_list "}" { RecordType l }

field_type_list:
    t=field_type { [t] }
  | t=field_type "," { [t] }
  | t=field_type "," l=field_type_list { t :: l }

field_type:
    m=is_mutable id=IDENT "=" t=type_expr { (m, id, t) }

is_mutable:
    { false }
  | "mutable" { true }

interface_type:
    "interface" "end" { InterfaceType [] }
  | "interface" l=method_type_list "end" { InterfaceType l }

method_type_list:
    t=method_type { [t] }
  | t=method_type "," { [t] }
  | t=method_type "," l=method_type_list { t :: l }

method_type:
    id=IDENT ":" t=must_function_type { (id, t) }

type_expr:
    t=type_infix_function { t }

type_infix_function:
    t=type_infix_tuple { t }
  | t=must_function_type { t }

must_function_type:
    a=type_infix_tuple "->" b=type_infix_function { FunctionType (a, b) }

type_infix_tuple:
    t=inner_type_expr { t }
  | t=inner_type_expr "*" l=type_tuple_list { TupleType (t::l) }

type_tuple_list:
    t=inner_type_expr { [t] }
  | t=inner_type_expr "*" l=type_tuple_list { t :: l }

inner_type_expr:
    t=type_terminal { t }
  | t=type_specialization { t }
  | "(" t=type_expr ")" { t }

highest_prec_type_expr:
    t=type_terminal { t }
  | "(" t=type_expr ")" { t }

type_specialization:
    t=highest_prec_type_expr id=IDENT { SpecializedType (t, id) }

type_terminal:
    id=IDENT { SingleType (TypeName id) }
  | t=type_symbol { SingleType t }

expr:
    e=local_expr { e }
  | e=if_expr { e }
  | e=match_expr { e }
  | e=lambda_expr { e }
  | e=infix_op { e }
  | "(" e=expr ":" t=type_expr ")" { ExprWithType (e, t) }

local_expr:
    d=let_decl "in" e=expr { Local (d, e) }

if_expr:
    "if" cond=expr "then" e=expr { IfExpr (cond, e, None) }
  | "if" cond=expr "then" a=expr "else" b=expr { IfExpr (cond, a, Some b) }

match_expr:
    "match" e=expr "with" l=match_list { MatchExpr (e, l) }
  | "match" e=expr "with" "|" l=match_list { MatchExpr (e, l) }

match_list:
    b=match_branch %prec LAST_BRANCH { [b] }
  | b=match_branch "|" l=match_list { b :: l }

match_branch:
    p=pattern "=>" e=expr { (p, e) }

lambda_expr:
    e=fun_expr { e }
  | e=function_expr { e }

fun_expr:
    "fun" proto=prototype "=>" e=expr { val_of_fun e proto }

function_expr:
    "function" l=match_list { LambdaExpr l }
  | "function" "|" l=match_list { LambdaExpr l }

pattern:
    p=single_pattern { p }
  | a=single_pattern "|" b=pattern { ManyPattern (a, b) }

single_pattern:
    p=pattern_infix { p }
  | p=pattern_with_type { p }

pattern_with_type:
    "(" p=pattern ":" t=type_expr ")" { PatternWithType (p, t) }

pattern_infix:
    p=pattern_infix_cons { p }

pattern_infix_cons:
    p=inner_pattern { p }
  | hd=inner_pattern "::" tl=pattern_infix_cons { ConsPattern (hd, tl) }

inner_pattern:
    p=pattern_terminal { p }
  | p=variant_pattern { p }
  | "(" p=pattern ")" { p }

highest_prec_pattern:
    p=pattern_terminal { p }
  | "(" p=pattern ")" { p }

pattern_terminal:
    p=pattern_literal { p }
  | p=unit_pattern { p }
  | p=tuple_pattern { p }
  | p=list_pattern { p }
  | p=array_pattern { p }
  | p=record_pattern { p }
  | p=variable_pattern { p }

pattern_literal:
    b=BOOL { BoolPattern b }
  | i=INT { IntPattern i }
  | "-" i=INT { IntPattern (-i) }
  | c=CHAR { CharPattern c }
  | s=STRING { StringPattern s }

unit_pattern:
    unit { UnitPattern }

tuple_pattern:
    "(" p=pattern "," l=pattern_item_list ")" { TuplePattern (p::l) }

list_pattern:
    "[" "]" { ListPattern [] }
  | "[" l=pattern_item_list "]" { ListPattern l }

array_pattern:
    "[|" "|]" { ArrayPattern [] }
  | "[|" l=pattern_item_list "|]" { ArrayPattern l }

pattern_item_list:
    p=pattern { [p] }
  | p=pattern "," l=pattern_item_list { p :: l }

record_pattern:
    "{" "}" { RecordPattern [] }
  | "{" l=field_pattern_list "}" { RecordPattern l }

field_pattern_list:
    p=field_pattern { [p] }
  | p=field_pattern "," { [p] }
  | p=field_pattern "," l=field_pattern_list { p :: l }

field_pattern:
    id=IDENT "=" p=pattern { (id, p) }

variable_pattern:
    id=IDENT { VariablePattern id }
  | WILDCARD { Wildcard }

variant_pattern:
    cid=CAPID { VariantPattern (cid, None) }
  | cid=CAPID p=pattern_terminal { VariantPattern (cid, Some p) }
  | cid=CAPID "(" p=pattern ")" { VariantPattern (cid, Some p) }

infix_op:
    e=infix_assign { e }

infix_assign:
    e=infix_or { e }
  | b=binding ":=" e=infix_assign
    {
      match b with
      | Variable v -> AssignRef (v, e)
      | _ -> failwith "infix_assign"
    }
  | b=binding "<-" e=infix_assign
    {
      match b with
      | Variable v -> Assign (v, e)
      | _ -> failwith "infix_assign"
    }

infix_or:
    e=infix_and { e }
  | a=infix_or "||" b=infix_and { Binary (Or, a, b) }

infix_and:
    e=infix_cmp { e }
  | a=infix_and "&&" b=infix_cmp { Binary (And, a, b) }

infix_cmp:
    e=infix_cons { e }
  | a=infix_cmp "<"  b=infix_append { Binary (Lt, a, b) }
  | a=infix_cmp "<=" b=infix_append { Binary (Lte, a, b) }
  | a=infix_cmp ">"  b=infix_append { Binary (Gt, a, b) }
  | a=infix_cmp ">=" b=infix_append { Binary (Gte, a, b) }
  | a=infix_cmp "="  b=infix_append { Binary (Eq, a, b) }
  | a=infix_cmp "!=" b=infix_append { Binary (Neq, a, b) }

infix_append:
    e=infix_cons { e }
  | a=infix_append "@" b=infix_cons { Binary (Append, a, b) }

infix_cons:
    e=infix_concat { e }
  | a=infix_concat "::" b=infix_cons { Binary (Cons, a, b) }

infix_concat:
    e=infix_plus { e }
  | a=infix_concat "^" b=infix_plus { Binary (Concat, a, b) }

infix_plus:
    e=infix_times { e }
  | a=infix_plus "+" b=infix_times { Binary (Plus, a, b) }
  | a=infix_plus "-" b=infix_times { Binary (Minus, a, b) }

infix_times:
    e=prefix_minus { e }
  | a=infix_times "*" b=prefix_minus { Binary (Times, a, b) }
  | a=infix_times "/" b=prefix_minus { Binary (Div, a, b) }
  | a=infix_times "%" b=prefix_minus { Binary (Mod, a, b) }

prefix_minus:
    e=inner_expr { e }
  | "-" e=prefix_minus { Unary (Negative, e) }
  | "+" e=prefix_minus { Unary (Positive, e) }
  | "!" e=prefix_minus { Unary (Deref, e) }

inner_expr:
    e=highest_prec { e }
  | e=call { e }
  | e=construction { e }

highest_prec:
    e=terminal { e }
  | "(" e=expr_list ")" { e }

expr_list:
    e=expr { e }
  | a=expr ";" b=expr_list { ManyExpr (a, b) }

terminal:
    e=literal { e }
  | e=binding { e }
  | e=unit { e }
  | e=tuple { e }
  | e=list { e }
  | e=array { e }
  | e=record { e }

literal:
    b=BOOL { Bool b }
  | i=INT { Int i }
  | c=CHAR { Char c }
  | s=STRING { String s }

binding:
    id=IDENT { Variable (Expr (None, id)) }
  | e=highest_prec "." id=IDENT { Variable (Expr (Some e, id)) }
  | m=module_binding "." id=IDENT { Variable (Module (m, id)) }

module_binding:
    cid=CAPID { LeafModule cid }
  | m=module_binding "." cid=CAPID { SubModule (m, cid) }

unit:
    "(" ")" { Unit }

tuple:
    "(" e=expr "," l=item_list ")" { Tuple (e :: l) }

list:
    "[" "]" { List [] }
  | "[" l=item_list "]" { List l }

array:
    "[|" "|]" { Array [] }
  | "[|" l=item_list "|]" { Array l }

item_list:
    e=expr { [e] }
  | e=expr "," l=item_list { e :: l }

record:
    "{" "}" { Record [] }
  | "{" l=field_list "}" { Record l }

field_list:
    f=field_binding { [f] }
  | f=field_binding "," { [f] }
  | f=field_binding "," l=field_list { f :: l }

field_binding:
    id=IDENT "=" e=expr { (id, e) }

call:
    caller=caller e=highest_prec { Call (caller, e) }

caller:
    e=highest_prec { e }
  | e=call { e }

construction:
    cid=CAPID e=highest_prec { Construct (cid, e) }

%%