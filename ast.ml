(* AST definitions *)
type decl =
  | TypeBindings of type_bindings
  | ValueBindings of value_bindings
  | MethodBindings of method_bindings

and type_bindings = bool (* recursive *) * type_binding list
and type_binding = type_variable option * string (* id *) * type_construct

and type_variable = string list

and type_construct =
  | TypeExpr of type_expr
  | VariantsType of variants_type
  | RecordType of record_type
  | InterfaceType of interface_type

and type_expr =
  | SingleType of type_name
  | TupleType of type_expr list
  | FunctionType of type_expr * type_expr
  | SpecificType of type_expr list * string

and type_name =
  | TypeSymbol of string
  | TypeName of string

and variants_type = (string (* constructor *) * type_expr option) list
and record_type = (string (* field name *) * type_expr) list
and interface_type = (string (* signature *) * type_expr (* type *) ) list

and value_bindings = bool (* recursive *) * value_binding list
and value_binding = pattern * expr

and method_bindings = bool * method_binding list

and method_binding = pattern (* receiver *) * (pattern * expr)

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
  | RefPattern of pattern
  | Wildcard
  | VariantsPattern of string (* constructor *) * pattern option (* value *)
  | PatternList of pattern list
  | PatternWithType of pattern * type_expr

and record_pattern = (string * pattern) list

and expr =
  | Unit
  | Bool of bool
  | Int of int
  | Char of char
  | String of string
  | CapIdent of string
  | Variable of variable
  | Assign of variable * expr
  | Tuple of expr list
  | List of expr list
  | Array of expr list
  | Record of record_expr
  | Call of expr * expr
  | Unary of unary_op * expr
  | Binary of binary_op * expr * expr
  | Local of value_bindings * expr
  | IfExpr of if_expr
  | MatchExpr of match_expr
  | LambdaExpr of pattern * expr
  | ExprList of expr list
  | ExprWithType of expr * type_expr

and variable = expr option * string

and record_expr = (string * expr) list 

and unary_op = 
  | Positive | Negative | Deref

and binary_op =
  | Plus | Minus | Times | Div | Mod
  | Lt | Lte | Gt | Gte | Eq | Neq
  | And | Or | Cons | Append | Concat

and if_expr = expr (* condition *) * expr (* then *) * expr option (* else *)
and match_expr = expr * match_branch list (* matching branches *)
and match_branch = pattern * expr

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
  | TypeBindings b -> type_bindings_to_rep b
  | ValueBindings b -> value_bindings_to_rep b
  | MethodBindings b -> method_bindings_to_rep b

and type_bindings_to_rep ((r, l) : type_bindings) =
  let binding_to_rep (typvars_opt, id, construct) =
    match typvars_opt with
    | None -> 
      let id_rep = OneLine ("name", id) in
      let construct_rep = type_construct_to_rep construct in
      ManyLines ("binding", [id_rep; construct_rep])
    | Some vars ->
      let vars_rep = type_vars_to_rep vars in
      let id_rep = OneLine ("name", id) in
      let construct_rep = type_construct_to_rep construct in
      ManyLines ("binding", [vars_rep; id_rep; construct_rep])
  in
  let name = "type " ^ if r then "rec" else "nonrec" in
  let bindings = List.map binding_to_rep l in
  ManyLines (name, bindings)

and type_vars_to_rep = function
  | [] -> failwith "type_vars_to_rep"
  | [x] -> OneLine ("var", "'" ^ x)
  | l ->
    let aux x =
      OneLine ("vars", "'" ^ x)
    in ManyLines ("vars", (List.map aux l))

and type_construct_to_rep = function
  | TypeExpr t -> type_expr_to_rep t
  | VariantsType t -> variant_type_to_rep t
  | RecordType t -> record_type_to_rep t
  | InterfaceType t -> interface_type_to_rep t

and type_expr_to_rep = function
  | SingleType t ->
    OneLine ("single-type", match t with TypeName s -> s | TypeSymbol s -> "'" ^ s)
  | TupleType l ->
    ManyLines ("tuple-type", List.map type_expr_to_rep l)
  | FunctionType (arg, body) ->
    ManyLines ("function-type", [type_expr_to_rep arg; type_expr_to_rep body])
  | SpecificType (args, name) ->
    let typevars = ManyLines ("type-variables", List.map type_expr_to_rep args) in
    ManyLines ("specific-type", [typevars; OneLine ("generics-type", name)])

and variant_type_to_rep l =
  let aux = function
    | (name, None) -> OneLine ("variant", name)
    | (name, Some typ) -> ManyLines ("variant", [OneLine ("constructor", name); type_expr_to_rep typ])
  in ManyLines ("variants-type", List.map aux l)

and record_type_to_rep l =
  let aux (name, typ) =
    ManyLines ("field", [OneLine ("name", name); type_expr_to_rep typ])
  in ManyLines ("record-type", List.map aux l)

and interface_type_to_rep l =
  let aux (name, typ) =
    ManyLines ("signature", [OneLine ("name", name); type_expr_to_rep typ])
  in ManyLines ("interface-type", List.map aux l)

and value_bindings_to_rep ((r, l) : value_bindings) =
  let binding_to_rep (pattern, expr) =
    let pattern_rep = pattern_to_rep pattern in
    let expr_rep = expr_to_rep expr in
    ManyLines ("binding", [pattern_rep; expr_rep])
  in
  let name = "value " ^ if r then "rec" else "nonrec" in
  let bindings = List.map binding_to_rep l in
  ManyLines (name, bindings)

and method_bindings_to_rep ((r, l) : method_bindings) =
  let binding_to_rep (receiver, (pattern, expr)) = 
    let receiver_rep = pattern_to_rep receiver in
    let pattern_rep = pattern_to_rep pattern in
    let expr_rep = expr_to_rep expr in
    ManyLines ("binding", [receiver_rep; pattern_rep; expr_rep])
  in
  let name = "method " ^ if r then "rec" else "nonrec" in
  let bindings = List.map binding_to_rep l in
  ManyLines (name, bindings)

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
  | RefPattern p -> ManyLines ("ref-pattern", [pattern_to_rep p])
  | Wildcard -> OneLine ("wildcard", "_")
  | VariantsPattern (name, pat_opt) -> begin
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
  | CapIdent name -> OneLine ("capital-ident", name)
  | Variable v -> variable_to_rep v
  | Assign (v, e) -> ManyLines ("assignment", [variable_to_rep v; expr_to_rep e])
  | Tuple l -> ManyLines ("tuple", List.map expr_to_rep l)
  | List l -> ManyLines ("list", List.map expr_to_rep l)
  | Array l -> ManyLines ("array", List.map expr_to_rep l)
  | Record e -> record_expr_to_rep e
  | Call (caller, e) -> ManyLines ("call", [expr_to_rep caller; expr_to_rep e])
  | Unary (op, e) -> unary_op_to_rep (op, e)
  | Binary (op, a, b) -> binary_op_to_rep (op, a, b)
  | Local (b, e) -> ManyLines ("local", [value_bindings_to_rep b; expr_to_rep e])
  | IfExpr e -> if_expr_to_rep e
  | MatchExpr e -> match_expr_to_rep e
  | LambdaExpr (arg, body) -> lambda_expr_to_rep (arg, body)
  | ExprList l -> ManyLines ("multi-expr", List.map expr_to_rep l)
  | ExprWithType (e, typ) -> ManyLines ("expr-with-type", [expr_to_rep e; type_expr_to_rep typ])
 
and variable_to_rep = function
  | (None, name) -> OneLine ("variable", name)
  | (Some e, name) -> ManyLines ("variable", [expr_to_rep e; OneLine("name", name)])

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

and lambda_expr_to_rep (arg, body) =
  ManyLines ("function",
    [ ManyLines ("arg", [pattern_to_rep arg])
    ; ManyLines ("body", [expr_to_rep body])
    ])

and branch_to_rep (p, e) =
  ManyLines ("branch", [pattern_to_rep p; expr_to_rep e])