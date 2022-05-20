type t = Num of int | Bool of bool | Char of char
[@@deriving sexp, compare, equal]