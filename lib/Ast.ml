open Ppx_compare_lib.Builtin
open Sexplib.Std

type literal = Int of int [@@deriving sexp, compare]
type unary_op = Neg | Bang | Tilde [@@deriving sexp, compare]

type binary_op =
  | Plus
  | Minus
  | Star
  | Slash
  | LAnd
  | LOr
  | Eq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte
[@@deriving sexp, compare]

type _type = U32 [@@deriving sexp, compare]

type identifier = string [@@deriving sexp, compare]

type expr =
  | Literal of literal
  | Unary of unary_expr
  | Binary of binary_expr
  | Var of identifier
  | Conditional of expr * expr * expr
  | Call of expr * expr list

and unary_expr = unary_op * expr
and binary_expr = expr * binary_op * expr
and var_expr = identifier [@@deriving sexp, compare]

type stmt =
  | Declaration of identifier * _type option * expr option
  | Expr of expr
  | Block of stmt list
  | Selection of expr * stmt * stmt option
  | While of expr * stmt
  | Do of stmt * expr
  | For of identifier * expr
  | Return of expr option
  | Break
  | Continue [@@deriving sexp, compare]

type typed_var = identifier * _type [@@deriving sexp, compare]

type func = identifier * typed_var list * _type option * stmt [@@deriving sexp, compare]
