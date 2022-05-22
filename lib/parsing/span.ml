open Base

type lexing_pos = [%import: Lexing.position] [@@deriving sexp, compare, equal, show]
type t = lexing_pos * lexing_pos [@@deriving sexp, compare, equal, show]

let new_t () =
  let new_pos = { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 } in
  (new_pos, new_pos)

let to_string t = Sexp.to_string (sexp_of_t t)
