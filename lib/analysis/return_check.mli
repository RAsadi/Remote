open Typing
open Base

val check_translation_unit :
  Typed_ast.translation_unit -> Typed_ast.translation_unit Or_error.t
