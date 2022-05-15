open Compile
open Core

let usage_msg = "rc <files...>"
let input_files = ref []
let add_filename filename = input_files := filename :: !input_files

let () =
  Arg.parse [] add_filename usage_msg;
  List.iter !input_files ~f:compile_file
