open Ppx_compare_lib.Builtin
open Sexplib.Std

type t = string [@@deriving sexp, compare, equal, show]