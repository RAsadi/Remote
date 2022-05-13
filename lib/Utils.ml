open Core
open Lexing
open Printer
open Backend

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
  let instrs = gen_translation_unit ast in
  let filename =
    Stdlib.Filename.remove_extension (Stdlib.Filename.basename filename)
  in
  (match Stdlib.Sys.command "mkdir -p build" with
  | 0 -> ()
  | _ ->
      Stdio.prerr_endline "Failed to create build folder";
      Caml.exit 1);
  (* output our asm into the build folder *)
  (* TODO get file names from somewhere *)
  let out_ch = Out_channel.create ("build/" ^ filename ^ ".s") in
  List.iter instrs ~f:(fun instr ->
      Out_channel.output_string out_ch @@ Instr.to_string instr ^ "\n");
  Out_channel.flush out_ch;
  (match
     Stdlib.Sys.command
       ("as -arch arm64 -o build/" ^ filename ^ ".o build/" ^ filename ^ ".s ")
   with
  | 0 -> ()
  | _ ->
      Stdio.prerr_endline "Failed to assemble";
      Caml.exit 1);
  match
    Stdlib.Sys.command
      ("ld -o build/" ^ filename ^ " build/" ^ filename
     ^ ".o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start \
        -arch arm64")
  with
  | 0 -> ()
  | _ ->
      Stdio.prerr_endline "Failed to link";
      Caml.exit 1
