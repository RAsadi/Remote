type t = Num of int | Bool of bool | Char of char | String of string
[@@deriving sexp, compare, equal]