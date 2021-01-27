open Types

(* Typed AST definitions *)
type decl = ValueBindings of value_bindings

and value_bindings = bool (* recursive *) * value_binding list

and value_binding = string list (* variable names *) * pattern * expr

and pattern =
  | UnitPattern     of datatype
  | BoolPattern     of bool * datatype
  | IntPattern      of int * datatype
  | CharPattern     of char * datatype
  | StringPattern   of string * datatype
  | TuplePattern    of pattern list * datatype
  | ListPattern     of pattern list * datatype
  | ConsPattern     of pattern * pattern * datatype
  | ArrayPattern    of pattern list * datatype
  | RecordPattern   of record_pattern * datatype
  | VariablePattern of string * datatype
  | RefPattern      of pattern * datatype
  | Wildcard        of datatype
  | VariantPattern  of variant * pattern option * datatype
  | PatternList     of pattern list * datatype
  | PatternWithType of pattern * datatype

and record_pattern = (field * pattern) list

and expr =
  | Unit         of datatype
  | Bool         of bool * datatype
  | Int          of int * datatype
  | Char         of char * datatype
  | String       of string * datatype
  | Variant      of variant * datatype
  | Construct    of variant * expr * datatype
  | Variable     of variable
  | Assign       of expr * expr * datatype
  | Tuple        of expr list * datatype
  | List         of expr list * datatype
  | Array        of expr list * datatype
  | Record       of record_expr
  | Call         of expr * expr * datatype
  | Unary        of Ast.unary_op * expr * datatype
  | Binary       of Ast.binary_op * expr * expr * datatype
  | Local        of value_bindings * expr * datatype
  | IfExpr       of if_expr
  | MatchExpr    of match_expr
  | LambdaExpr   of lambda_expr
  | ExprList     of expr list * datatype
  | ExprWithType of expr * datatype

and variable = expr option * string * datatype

and record_expr = (string * expr) list * datatype

and if_expr =
  expr (* cond *) * expr (* then *) * expr option (* else *) * datatype

and match_expr = expr * match_branch list (* matching branches *) * datatype

and match_branch = string list (* variable names *) * pattern * expr * datatype

and lambda_expr = string list * pattern * expr * datatype

let rec update_typed_ast tvtab = function
  | ValueBindings bindings -> update_value_bindings tvtab bindings

and update_value_bindings tvtab (r, l) =
  let update_value_binding (names, p, e) =
    (names, update_pattern tvtab p, update_expr tvtab e)
  in
  (r, List.map update_value_binding l)

and update_pattern tvtab = function
  | UnitPattern t -> UnitPattern (apply_real_type tvtab t)
  | BoolPattern (b, t) -> BoolPattern (b, apply_real_type tvtab t)
  | IntPattern (i, t) -> IntPattern (i, apply_real_type tvtab t)
  | CharPattern (c, t) -> CharPattern (c, apply_real_type tvtab t)
  | StringPattern (s, t) -> StringPattern (s, apply_real_type tvtab t)
  | TuplePattern (l, t) ->
      TuplePattern (List.map (update_pattern tvtab) l, apply_real_type tvtab t)
  | ListPattern (l, t) ->
      ListPattern (List.map (update_pattern tvtab) l, apply_real_type tvtab t)
  | ConsPattern (a, b, t) ->
      ConsPattern
        (update_pattern tvtab a, update_pattern tvtab b, apply_real_type tvtab t)
  | ArrayPattern (l, t) ->
      ArrayPattern (List.map (update_pattern tvtab) l, apply_real_type tvtab t)
  | RecordPattern (l, t) -> failwith "update_pattern"
  | VariablePattern (name, t) -> VariablePattern (name, apply_real_type tvtab t)
  | RefPattern (v, t) ->
      RefPattern (update_pattern tvtab v, apply_real_type tvtab t)
  | Wildcard t -> Wildcard (apply_real_type tvtab t)
  | VariantPattern (v, None, t) ->
      VariantPattern (v, None, apply_real_type tvtab t)
  | VariantPattern (v, Some p, t) ->
      VariantPattern (v, Some (update_pattern tvtab p), apply_real_type tvtab t)
  | PatternList (l, t) ->
      PatternList (List.map (update_pattern tvtab) l, apply_real_type tvtab t)
  | PatternWithType (p, t) ->
      PatternWithType (update_pattern tvtab p, apply_real_type tvtab t)

and update_expr tvtab = function
  | Unit t -> Unit (apply_real_type tvtab t)
  | Bool (b, t) -> Bool (b, apply_real_type tvtab t)
  | Int (i, t) -> Int (i, apply_real_type tvtab t)
  | Char (c, t) -> Char (c, apply_real_type tvtab t)
  | String (s, t) -> String (s, apply_real_type tvtab t)
  | Variant (v, t) -> Variant (v, apply_real_type tvtab t)
  | Construct (v, e, t) ->
      Construct (v, update_expr tvtab e, apply_real_type tvtab t)
  | Variable v -> update_variable tvtab v
  | Assign (d, s, t) ->
      Assign (update_expr tvtab d, update_expr tvtab s, apply_real_type tvtab t)
  | Tuple (l, t) ->
      Tuple (List.map (update_expr tvtab) l, apply_real_type tvtab t)
  | List (l, t) -> List (List.map (update_expr tvtab) l, apply_real_type tvtab t)
  | Array (l, t) ->
      Array (List.map (update_expr tvtab) l, apply_real_type tvtab t)
  | Record (l, t) -> failwith "update_expr"
  | Call (clr, cle, t) ->
      Call
        (update_expr tvtab clr, update_expr tvtab cle, apply_real_type tvtab t)
  | Unary (op, v, t) -> Unary (op, update_expr tvtab v, apply_real_type tvtab t)
  | Binary (op, a, b, t) ->
      Binary
        (op, update_expr tvtab a, update_expr tvtab b, apply_real_type tvtab t)
  | Local (b, e, t) ->
      Local
        ( update_value_bindings tvtab b
        , update_expr tvtab e
        , apply_real_type tvtab t )
  | IfExpr e -> update_if_expr tvtab e
  | MatchExpr e -> update_match_expr tvtab e
  | LambdaExpr (names, p, e, t) ->
      LambdaExpr
        ( names
        , update_pattern tvtab p
        , update_expr tvtab e
        , apply_real_type tvtab t )
  | ExprList (l, t) ->
      ExprList (List.map (update_expr tvtab) l, apply_real_type tvtab t)
  | ExprWithType (e, t) ->
      ExprWithType (update_expr tvtab e, apply_real_type tvtab t)

and update_variable tvtab = function
  | None, name, typ -> Variable (None, name, apply_real_type tvtab typ)
  | Some e, name, typ ->
      Variable (Some (update_expr tvtab e), name, apply_real_type tvtab typ)

and update_if_expr tvtab = function
  | cond, then_, None, typ ->
      IfExpr
        ( update_expr tvtab cond
        , update_expr tvtab then_
        , None
        , apply_real_type tvtab typ )
  | cond, then_, Some else_, typ ->
      IfExpr
        ( update_expr tvtab cond
        , update_expr tvtab then_
        , Some (update_expr tvtab else_)
        , apply_real_type tvtab typ )

and update_match_expr tvtab (e, l, typ) =
  let e = update_expr tvtab e in
  let aux (names, p, e, t) =
    (names, update_pattern tvtab p, update_expr tvtab e, apply_real_type tvtab t)
  in
  let l = List.map aux l in
  MatchExpr (e, l, apply_real_type tvtab typ)
;;
