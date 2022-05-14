open Ppx_compare_lib.Builtin
open Sexplib.Std

type lexing_pos = [%import: Lexing.position] [@@deriving sexp, compare]
type t = lexing_pos * lexing_pos [@@deriving sexp, compare]
