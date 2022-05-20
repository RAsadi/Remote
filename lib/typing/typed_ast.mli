module Expr : sig
  type t =
    | Literal of Parsing.Span.t * Ast.Type.t * Ast.Literal.t
    | Unary of Parsing.Span.t * Ast.Type.t * Ast.Operator.unary * t
    | Sizeof of Parsing.Span.t * Ast.Type.t * Ast.Type.t
    | Binary of Parsing.Span.t * Ast.Type.t * t * Ast.Operator.binary * t
    | Var of Parsing.Span.t * Ast.Type.t * string
    | Call of Parsing.Span.t * Ast.Type.t * string * t list
    | PostFix of Parsing.Span.t * Ast.Type.t * t * Ast.Operator.postfix
    | FieldAccess of Parsing.Span.t * Ast.Type.t * t * string
    | Initializer of Parsing.Span.t * Ast.Type.t * string * t list

  val get_type : t -> Ast.Type.t
end

module Stmt : sig
  type t =
    | Declaration of declaration
    | Assignment of Parsing.Span.t * Expr.t * Expr.t
    | Expr of Parsing.Span.t * Expr.t
    | Block of Parsing.Span.t * t list
    | If of Parsing.Span.t * Expr.t * t * t option
    | While of Parsing.Span.t * Expr.t * t
    | For of Parsing.Span.t * string * Expr.t * t
    | Return of Parsing.Span.t * Expr.t option
    | Break of Parsing.Span.t
    | Continue of Parsing.Span.t

  and declaration = {
    span : Parsing.Span.t;
    mut : Ast.Type.mutability;
    id : string;
    type_annotation : Ast.Type.t option;
    defn : Expr.t option;
  }
end

type typed_var = Parsing.Span.t * string * Ast.Type.t

module Fn : sig
  type t = {
    span : Parsing.Span.t;
    id : string;
    args : typed_var list;
    typ : Ast.Type.t;
    body : Stmt.t;
  }
end

module Struct : sig
  type t = Parsing.Span.t * string * typed_var list
end

module TopLevelElement : sig
  type t = Fn of Fn.t | Struct of Struct.t
end

type translation_unit = TopLevelElement.t list [@@deriving sexp, compare, equal]

val add_struct :
  (string, (string * Ast.Type.t) list, 'a) Base.Map.t ->
  Parsing.Parsed_ast.Struct.t ->
  (string, (string * Ast.Type.t) list, 'a) Base.Map.t

val get_struct_map :
  TopLevelElement.t list ->
  ( string,
    (string * Ast.Type.t) list,
    Base.String.comparator_witness )
  Base.Map.t
