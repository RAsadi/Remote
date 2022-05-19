open Ppx_compare_lib.Builtin
open Sexplib.Std

type t = Num of int | Bool of bool | Char of char
[@@deriving sexp, compare, equal]