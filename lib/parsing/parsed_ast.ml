open Ppx_compare_lib.Builtin
open Sexplib.Std
open Ast

module Expr : sig
  type t =
    | Literal of Span.t * Literal.t
    | Unary of Span.t * Operator.unary * t
    | Sizeof of Span.t * Type.t
    | Binary of Span.t * t * Operator.binary * t
    | Var of Span.t * Identifier.t
    | Call of Span.t * Identifier.t * t list
    | PostFix of Span.t * t * Operator.postfix
    | FieldAccess of Span.t * t * Identifier.t
    | Initializer of Span.t * Identifier.t * t list
    | Cast of Span.t * t * Type.t
  [@@deriving sexp, compare, equal]
end = struct
  type t =
    | Literal of Span.t * Literal.t
    | Unary of Span.t * Operator.unary * t
    | Sizeof of Span.t * Type.t
    | Binary of Span.t * t * Operator.binary * t
    | Var of Span.t * Identifier.t
    | Call of Span.t * Identifier.t * t list
    | PostFix of Span.t * t * Operator.postfix
    | FieldAccess of Span.t * t * Identifier.t
    | Initializer of Span.t * Identifier.t * t list
    | Cast of Span.t * t * Type.t
  [@@deriving sexp, compare, equal]
end

module Stmt : sig
  type t =
    | Declaration of declaration
    | Assignment of Span.t * Expr.t * Expr.t
    | Expr of Span.t * Expr.t
    | Block of Span.t * t list
    | If of Span.t * Expr.t * t * t option
    | While of Span.t * Expr.t * t
    | For of Span.t * t * Expr.t * Expr.t * t
    | Return of Span.t * Expr.t option
    | Break of Span.t
    | Continue of Span.t

  and declaration = {
    span : Span.t;
    mut : Type.mutability;
    id : Identifier.t;
    type_annotation : Type.t option;
    defn : Expr.t option;
  }
  [@@deriving sexp, compare, equal]
end = struct
  type t =
    | Declaration of declaration
    | Assignment of Span.t * Expr.t * Expr.t
    | Expr of Span.t * Expr.t
    | Block of Span.t * t list
    | If of Span.t * Expr.t * t * t option
    | While of Span.t * Expr.t * t
    | For of Span.t * t * Expr.t * Expr.t * t
    | Return of Span.t * Expr.t option
    | Break of Span.t
    | Continue of Span.t

  and declaration = {
    span : Span.t;
    mut : Type.mutability;
    id : Identifier.t;
    type_annotation : Type.t option;
    defn : Expr.t option;
  }
  [@@deriving sexp, compare, equal]
end

type typed_var = Span.t * Identifier.t * Type.t
[@@deriving sexp, compare, equal]

module Fn : sig
  type t = {
    span : Span.t;
    id : Identifier.t;
    args : typed_var list;
    typ : Type.t;
    body : Stmt.t;
  }
  [@@deriving sexp, compare, equal]
end = struct
  type t = {
    span : Span.t;
    id : Identifier.t;
    args : typed_var list;
    typ : Type.t;
    body : Stmt.t;
  }
  [@@deriving sexp, compare, equal]
end

module Struct : sig
  type t = Span.t * Identifier.t * typed_var list
  [@@deriving sexp, compare, equal]
end = struct
  type t = Span.t * Identifier.t * typed_var list
  [@@deriving sexp, compare, equal]
end

module TopLevelElement : sig
  type t = Fn of Fn.t | Struct of Struct.t [@@deriving sexp, compare, equal]
end = struct
  type t = Fn of Fn.t | Struct of Struct.t [@@deriving sexp, compare, equal]
end

type translation_unit = TopLevelElement.t list [@@deriving sexp, compare, equal]