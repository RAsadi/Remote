open Ppx_compare_lib.Builtin
open Sexplib.Std

type identifier = string [@@deriving sexp, compare, equal]

type literal = Num of int | Bool of bool | Char of char
[@@deriving sexp, compare, equal]

type _type = U32 | U8 | Bool | Void | Struct of identifier | Pointer of _type
[@@deriving sexp, compare, equal]

type mutability = Mut | Const [@@deriving sexp, compare, equal]
type unary_op = Neg | Bang | Tilde | Addr [@@deriving sexp, compare, equal]

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

type postfix_op = Incr | Decr | Deref [@@deriving sexp, compare, equal]
