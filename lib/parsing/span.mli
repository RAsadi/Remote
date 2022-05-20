type lexing_pos = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}

type t = lexing_pos * lexing_pos [@@deriving sexp, compare, equal]

val new_t : unit -> lexing_pos * lexing_pos
