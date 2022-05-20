open Parsing
open Base

val type_translation_unit :
  Parsed_ast.translation_unit -> Typed_ast.translation_unit Or_error.t
