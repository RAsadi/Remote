open Core
open Parsed_ast

let print_program translation_unit =
  let formatter = Format.formatter_of_out_channel stdout in
  Sexp.pp_hum formatter (sexp_of_translation_unit translation_unit);
  Format.pp_print_flush formatter ();
  print_endline ""
