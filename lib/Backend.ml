open Ast
open Base

type variable_mapping = (string, int, String.comparator_witness) Map.t

type exec_context = {
  var_map : variable_mapping;
  curr_scope : variable_mapping;
  continue_label : string option;
  break_label : string option;
  instrs : string list ref;
}

let gen_expr (ctx : exec_context) (expr : expr) : Instr.t list * int = ([], 0)
let gen_stmt (ctx : exec_context) (stmt : stmt) : Instr.t list * int = ([], 0)

let gen_translation_unit (trans : translation_unit) : Instr.t list =
  let premable : Instr.t list =
    [
      Raw ".data";
      Raw ".text";
      Raw ".globl _start";
      Raw ".align 4";
      Raw "_start:";
      Raw "bl main";
      Mov (Real X16, Const 1);
      Svc (Const 0);
    ]
  in
  premable
