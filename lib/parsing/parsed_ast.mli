module Expr : sig
  type t =
    | Literal of Span.t * Ast.Literal.t
    | Unary of Span.t * Ast.Operator.unary * t
    | Sizeof of Span.t * Ast.Type.t
    | Binary of Span.t * t * Ast.Operator.binary * t
    | Var of Span.t * string
    | Call of Span.t * string * t list
    | PostFix of Span.t * t * Ast.Operator.postfix
    | FieldAccess of Span.t * t * string
    | Initializer of Span.t * string * t list

  val t_of_sexp : Sexplib0.Sexp.t -> t
end

module Stmt : sig
  type t =
    | Declaration of declaration
    | Assignment of Span.t * Expr.t * Expr.t
    | Expr of Span.t * Expr.t
    | Block of Span.t * t list
    | If of Span.t * Expr.t * t * t option
    | While of Span.t * Expr.t * t
    | For of Span.t * string * Expr.t * t
    | Return of Span.t * Expr.t option
    | Break of Span.t
    | Continue of Span.t

  and declaration = {
    span : Span.t;
    mut : Ast.Type.mutability;
    id : string;
    type_annotation : Ast.Type.t option;
    defn : Expr.t option;
  }
end

type typed_var = Span.t * string * Ast.Type.t

module Fn : sig
  type t = {
    span : Span.t;
    id : string;
    args : typed_var list;
    typ : Ast.Type.t;
    body : Stmt.t;
  }
end

module Struct : sig
  type t = Span.t * string * typed_var list
end

module TopLevelElement : sig
  type t = Fn of Fn.t | Struct of Struct.t
end

type translation_unit = TopLevelElement.t list [@@deriving sexp, compare, equal]
