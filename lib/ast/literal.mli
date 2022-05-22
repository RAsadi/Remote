type t = Num of int | Bool of bool | Char of char | String of string | Null
[@@deriving sexp, compare, equal, show]