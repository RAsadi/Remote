open Ppx_compare_lib.Builtin
open Sexplib.Std
open SharedAst

type literal = U32 of int | Bool of bool [@@deriving sexp, compare, equal]
type identifier = string [@@deriving sexp, compare, equal]

type expr =
  | Literal of Span.t * _type * literal
  | Unary of unary_expr
  | Sizeof of Span.t * _type * identifier
  | Binary of binary_expr
  | Var of Span.t * _type * identifier
  | Call of Span.t * _type * identifier * expr list
  | PostFix of Span.t * _type * expr * postfix_op

and unary_expr = Span.t * _type * unary_op * expr

and binary_expr = Span.t * _type * expr * binary_op * expr
[@@deriving sexp, compare, equal]