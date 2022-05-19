open Ppx_compare_lib.Builtin
open Sexplib.Std
open Ast

type expr =
  | Literal of Span.t * Literal.t
  | Unary of unary_expr
  | Sizeof of Span.t * Type.t
  | Binary of binary_expr
  | Var of Span.t * Identifier.t
  | Call of Span.t * Identifier.t * expr list
  | PostFix of Span.t * expr * Operator.postfix
  | FieldAccess of Span.t * expr * Identifier.t
  | Initializer of Span.t * Identifier.t * expr list

and unary_expr = Span.t * Operator.unary * expr

and binary_expr = Span.t * expr * Operator.binary * expr
[@@deriving sexp, compare, equal]

type stmt =
  | Declaration of declaration
  | Assignment of Span.t * expr * expr
  | Expr of Span.t * expr
  | Block of Span.t * stmt list
  | If of Span.t * expr * stmt * stmt option
  | While of Span.t * expr * stmt
  | For of Span.t * Identifier.t * expr * stmt
  | Return of Span.t * expr option
  | Break of Span.t
  | Continue of Span.t

and declaration = {
  span : Span.t;
  mut : Type.mutability;
  id : Identifier.t;
  type_annotation : Type.t option;
  defn : expr option;
}
[@@deriving sexp, compare, equal]

type typed_var = Span.t * Identifier.t * Type.t
[@@deriving sexp, compare, equal]

type fn = {
  span : Span.t;
  id : Identifier.t;
  args : typed_var list;
  typ : Type.t;
  body : stmt;
}
[@@deriving sexp, compare, equal]

type _struct = Span.t * Identifier.t * typed_var list
[@@deriving sexp, compare, equal]

type top_level_element = Fn of fn | Struct of _struct
[@@deriving sexp, compare, equal]

type translation_unit = top_level_element list [@@deriving sexp, compare, equal]