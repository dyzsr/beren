(* AST definitions *)
type decl =
  | TypeBinding of type_binding
  | ValueBinding of value_binding
  | MethodBinding of method_binding

and type_binding = 
  bool (* recursive *) * (type_param option * string (* id *) * type_construct) list

and type_param = string list

and type_construct =
  | TypeExpr of type_expr
  | VariantType of variant_type
  | RecordType of record_type
  | InterfaceType of interface_type

and type_expr =
  | SingleType of type_name
  | TupleType of type_expr list
  | FunctionType of type_expr * type_expr
  | SpecializedType of type_expr * string

and variant_type = (string (* constructor *) * type_expr option) list
and record_type = (bool (* mutable *) * string (* id *) * type_expr) list
and interface_type = (string (* id *) * type_expr (* function type *) ) list

and type_name =
  | TypeSymbol of string
  | TypeName of string

and value_binding = bool (* recursive *) * (pattern * expr) list

and method_binding = pattern (* receiver *) * value_binding

and pattern =
  | BoolPattern of bool
  | IntPattern of int
  | CharPattern of char
  | StringPattern of string
  | UnitPattern
  | TuplePattern of pattern list (* (?, ?, ...) *)
  | ListPattern of pattern list (* [?, ?, ...] *)
  | ConsPattern of pattern * pattern (* ? :: ? *)
  | ArrayPattern of pattern list (* #[?, ?, ...] *)
  | RecordPattern of record_pattern (* {?=?, ...} *)
  | VariablePattern of string (* id *)
  | Wildcard
  | VariantPattern of string (* constructor *) * pattern option (* value *)
  | PatternList of pattern list
  | PatternWithType of pattern * type_expr

and record_pattern = (string * pattern) list

and expr =
  | Unit
  | Bool of bool
  | Int of int
  | Char of char
  | String of string
  | Variable of variable
  | Assign of variable * expr
  | AssignRef of variable * expr
  | Tuple of expr list
  | List of expr list
  | Array of expr list
  | Record of record_expr
  | Call of expr * expr
  | Construct of string * expr
  | Unary of unary_op * expr
  | Binary of binary_op * expr * expr
  | Local of value_binding * expr
  | IfExpr of if_expr
  | MatchExpr of match_expr
  | LambdaExpr of lambda_expr
  | ExprList of expr list
  | ExprWithType of expr * type_expr

and variable =
  | Expr of expr option * string
  | Module of module_binding * string

and module_binding =
  | LeafModule of string
  | SubModule of module_binding * string

and record_expr = (string * expr) list 

and unary_op = 
  | Positive | Negative | Deref

and binary_op =
  | Plus | Minus | Times | Div | Mod
  | Lt | Lte | Gt | Gte | Eq | Neq
  | And | Or | Cons | Append | Concat

and if_expr = expr (* condition *) * expr (* then *) * expr option (* else *)

and match_expr = expr * (pattern * expr) list (* matching branches *)

and lambda_expr = (pattern * expr) list (* function branches *)

type prototype = Prototype of pattern list * type_expr option


(* Visualization of AST *)
type rep =
  | OneLine of string * string
  | ManyLines of string * rep list

let rep_to_string rep =
  let add ident = ident ^ ": " in
  let rec aux indent = function
    | OneLine (name, text) -> indent ^ name ^ " " ^ text
    | ManyLines (name, l) ->
      let first_line = indent ^ name in
      let rest_lines = List.map (aux (add indent)) l in
      String.concat "\n" (first_line::rest_lines)
  in aux "" rep

let rec decl_to_rep = function
  | TypeBinding b -> type_binding_to_rep b
  | ValueBinding b -> value_binding_to_rep b
  | MethodBinding b -> method_binding_to_rep b

and type_binding_to_rep ((r, l) : type_binding) =
  let binding_to_rep (params_opt, id, construct) =
    match params_opt with
    | None -> 
      let id_rep = OneLine ("name", id) in
      let construct_rep = type_construct_to_rep construct in
      ManyLines ("binding", [id_rep; construct_rep])
    | Some params ->
      let params_rep = type_params_to_rep params in
      let id_rep = OneLine ("name", id) in
      let construct_rep = type_construct_to_rep construct in
      ManyLines ("binding", [params_rep; id_rep; construct_rep])
  in
  let name = "type " ^ if r then "rec" else "nonrec" in
  let bindings = List.map binding_to_rep l in
  ManyLines (name, bindings)

and type_params_to_rep = function
  | [] -> failwith "type_params_to_rep"
  | [x] -> OneLine ("param", "'" ^ x)
  | l ->
    let aux x =
      OneLine ("params", "'" ^ x)
    in ManyLines ("params", (List.map aux l))

and type_construct_to_rep = function
  | TypeExpr t -> type_expr_to_rep t
  | VariantType t -> variant_type_to_rep t
  | RecordType t -> record_type_to_rep t
  | InterfaceType t -> interface_type_to_rep t

and type_expr_to_rep = function
  | SingleType t ->
    OneLine ("single-type", match t with TypeName s -> s | TypeSymbol s -> "'" ^ s)
  | TupleType l ->
    ManyLines ("tuple-type", List.map type_expr_to_rep l)
  | FunctionType (param, res) ->
    ManyLines ("function-type", [type_expr_to_rep param; type_expr_to_rep res])
  | SpecializedType (arg, name) ->
    ManyLines ("specialized-type", [type_expr_to_rep arg; OneLine ("generics-type", name)])

and variant_type_to_rep l =
  let aux = function
    | (name, None) -> OneLine ("variant", name)
    | (name, Some typ) -> ManyLines ("variant", [OneLine ("constructor", name); type_expr_to_rep typ])
  in ManyLines ("variant-type", List.map aux l)

and record_type_to_rep l =
  let aux (mut, name, typ) =
    ManyLines ("field", [OneLine ("name", name); type_expr_to_rep typ])
  in ManyLines ("record-type", List.map aux l)

and interface_type_to_rep l =
  let aux (name, typ) =
    ManyLines ("method", [OneLine ("name", name); type_expr_to_rep typ])
  in ManyLines ("interface-type", List.map aux l)

and value_binding_to_rep ((r, l) : value_binding) =
  let binding_to_rep (pattern, expr) =
    let pattern_rep = pattern_to_rep pattern in
    let expr_rep = expr_to_rep expr in
    ManyLines ("binding", [pattern_rep; expr_rep])
  in
  let name = "value " ^ if r then "rec" else "nonrec" in
  let bindings = List.map binding_to_rep l in
  ManyLines (name, bindings)

and method_binding_to_rep (p, (r, l) : method_binding) =
  let binding_rep = value_binding_to_rep (r, l) in
  ManyLines ("method", [pattern_to_rep p; binding_rep])

and pattern_to_rep = function
  | BoolPattern b -> OneLine ("bool-pattern", string_of_bool b)
  | IntPattern i -> OneLine ("int-pattern", string_of_int i)
  | CharPattern c -> OneLine ("char-pattern", String.make 1 c)
  | StringPattern s -> OneLine ("string-pattern", s)
  | UnitPattern -> OneLine ("unit-pattern", "()")
  | TuplePattern l -> ManyLines ("tuple-pattern", List.map pattern_to_rep l)
  | ListPattern l -> ManyLines ("list-pattern", List.map pattern_to_rep l)
  | ConsPattern (hd, tl) -> ManyLines ("cons-pattern", [pattern_to_rep hd; pattern_to_rep tl])
  | ArrayPattern l -> ManyLines ("array-pattern", List.map pattern_to_rep l)
  | RecordPattern p -> record_pattern_to_rep p (* {?=?, ...} *)
  | VariablePattern v -> OneLine ("variable-pattern", v) (* id *)
  | Wildcard -> OneLine ("wildcard", "_")
  | VariantPattern (name, pat_opt) -> begin
    match pat_opt with
      | None -> OneLine ("variant-pattern", name)
      | Some p -> ManyLines ("variant-pattern", [OneLine ("name", name); pattern_to_rep p])
    end
  | PatternList l -> ManyLines ("pattern-list", List.map pattern_to_rep l)
  | PatternWithType (p, typ) -> ManyLines ("pattern-with-type", [pattern_to_rep p; type_expr_to_rep typ])

and record_pattern_to_rep l =
  let field_pattern_to_rep (name, pattern) =
    ManyLines ("field", [OneLine ("name", name); pattern_to_rep pattern])
  in
  ManyLines ("record-pattern", List.map field_pattern_to_rep l)

and expr_to_rep = function
  | Unit -> OneLine ("unit", "()")
  | Bool b -> OneLine ("bool", string_of_bool b)
  | Int i -> OneLine ("int", string_of_int i)
  | Char c -> OneLine ("char", String.make 1 c)
  | String s -> OneLine ("string", s)
  | Variable v -> variable_to_rep v
  | Assign (v, e) -> ManyLines ("assignment", [variable_to_rep v; expr_to_rep e])
  | AssignRef (v, e) -> ManyLines ("assignment-ref", [variable_to_rep v; expr_to_rep e])
  | Tuple l -> ManyLines ("tuple", List.map expr_to_rep l)
  | List l -> ManyLines ("list", List.map expr_to_rep l)
  | Array l -> ManyLines ("array", List.map expr_to_rep l)
  | Record e -> record_expr_to_rep e
  | Call (caller, e) -> ManyLines ("call", [expr_to_rep caller; expr_to_rep e])
  | Construct (name, e) -> ManyLines ("construct", [OneLine ("constructor", name); expr_to_rep e])
  | Unary (op, e) -> unary_op_to_rep (op, e)
  | Binary (op, a, b) -> binary_op_to_rep (op, a, b)
  | Local (b, e) -> ManyLines ("local", [value_binding_to_rep b; expr_to_rep e])
  | IfExpr e -> if_expr_to_rep e
  | MatchExpr e -> match_expr_to_rep e
  | LambdaExpr e -> lambda_expr_to_rep e
  | ExprList l -> ManyLines ("multi-expr", List.map expr_to_rep l)
  | ExprWithType (e, typ) -> ManyLines ("expr-with-type", [expr_to_rep e; type_expr_to_rep typ])
 
and variable_to_rep = function
  | Expr (None, name) -> OneLine ("variable", name)
  | Expr (Some e, name) -> ManyLines ("variable", [expr_to_rep e; OneLine("name", name)])
  | Module (b, name) -> begin
    let rec module_to_rep = function
      | LeafModule name -> name
      | SubModule (parent, name) -> module_to_rep parent ^ "." ^ name
    in ManyLines ("variable", [OneLine ("module", module_to_rep b); OneLine("name", name)])
    end

and record_expr_to_rep l =
  let field_to_rep (name, e) =
    ManyLines ("field", [OneLine ("name", name); expr_to_rep e])
  in ManyLines ("record", List.map field_to_rep l)

and unary_op_to_rep (op, e) =
  let op_text = match op with
    | Positive -> "+"
    | Negative -> "-"
    | Deref -> "!"
  in ManyLines ("unary", [OneLine ("op", op_text); expr_to_rep e])

and binary_op_to_rep (op, a, b) =
  let op_text = match op with
    | Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/" | Mod -> "%"
    | Lt -> "<" | Lte -> "<=" | Gt -> ">" | Gte -> ">=" | Eq -> "=" | Neq -> "!="
    | And -> "&&" | Or -> "||" | Cons -> "::" | Append -> "@" | Concat -> "^"
  in ManyLines ("binary", [OneLine ("op", op_text); expr_to_rep a; expr_to_rep b])

and if_expr_to_rep = function
  | (cond, e1, None) ->
    ManyLines ("if-then", [expr_to_rep cond; expr_to_rep e1])
  | (cond, e1, Some e2) ->
    ManyLines ("if-then-else", [expr_to_rep cond; expr_to_rep e1; expr_to_rep e2])

and match_expr_to_rep (e, l) =
  ManyLines ("match", expr_to_rep e :: List.map branch_to_rep l)

and lambda_expr_to_rep l =
  ManyLines ("function", List.map branch_to_rep l)

and branch_to_rep (p, e) =
  ManyLines ("branch", [pattern_to_rep p; expr_to_rep e])