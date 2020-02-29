let rec lang = "ocaml"

type decl =
  | TypeBinding of type_binding
  | ValueBinding of value_binding
  | MethodBinding of method_binding

and type_binding = 
  bool (* recursive *) * (type_param * string (* id *) * type_construct) list

and type_param = type_name list

and type_construct =
  | TypeExpr of type_expr
  | VariantType of variant_type
  | RecordType of record_type
  | InterfaceType of interface_type

and type_expr =
  | SingleType of type_name
  | TupleType of type_name list
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
  | ManyPattern of pattern * pattern
  | PatternWithType of pattern * type_expr

and record_pattern = (string * pattern) list

and expr =
  | Unit
  | Bool of bool
  | Int of int
  | Char of char
  | String of string
  | Variable of expr option * identifier
  | Tuple of expr list
  | List of expr list
  | Array of expr list
  | Record of (string * expr) list
  | Call of expr * expr
  | Construct of expr * expr
  | Unary of unary_op * expr
  | Binary of binary_op * expr * expr
  | Local of value_binding * expr
  | IfExpr of if_expr
  | MatchExpr of match_expr
  | LambdaExpr of lambda_expr
  | ManyExpr of expr * expr
  | ExprWithType of expr * type_expr

and identifier =
  | Ident of string
  | CapId of string

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