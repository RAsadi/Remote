open Core
open Lexing
open Printer

let colnum pos = pos.pos_cnum - pos.pos_bol - 1

let pos_string pos =
  let l = string_of_int pos.pos_lnum and c = string_of_int (colnum pos + 1) in
  "line " ^ l ^ ", column " ^ c

let parse' f s =
  let lexbuf = Lexing.from_string s in
  try f Lexer.token lexbuf with
  | Parser.Error ->
      print_endline ("Parse error at " ^ pos_string lexbuf.lex_curr_p);
      exit 1
  | Failure f ->
      print_endline f;
      exit 1

let parse_program s = parse' Parser.translation_unit s

let parse_file filename =
  let file = In_channel.create filename in
  let input_lines = In_channel.input_all file in
  parse_program input_lines

let compile_file filename =
  let ast = parse_file filename in
  print_program ast;
