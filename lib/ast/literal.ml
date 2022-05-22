open Ppx_compare_lib.Builtin
open Sexplib.Std

type t = Num of int | Bool of bool | Char of char | String of string | Null
[@@deriving sexp, compare, equal, show]