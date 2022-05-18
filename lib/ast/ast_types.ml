open Ppx_compare_lib.Builtin
open Sexplib.Std

type identifier = string [@@deriving sexp, compare, equal]
type literal = U32 of int | Bool of bool [@@deriving sexp, compare, equal]

type _type = U32 | Bool | Void | Identifier of identifier
[@@deriving sexp, compare, equal]

type mutability = Mut | Const [@@deriving sexp, compare, equal]
type unary_op = Neg | Bang | Tilde [@@deriving sexp, compare, equal]

type binary_op =
  (* Arithmetic *)
  | Plus
  | Minus
  | Star
  | Slash
  (* Bitwise *)
  | LShift
  | RShift
  | And
  | Or
  | Xor
  | LAnd
  | LOr
  (* Equality *)
  | Eq
  | Neq
  (* Comparison *)
  | Lt
  | Lte
  | Gt
  | Gte
[@@deriving sexp, compare, equal]

type postfix_op = Incr | Decr | Dot [@@deriving sexp, compare, equal]
