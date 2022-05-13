module Register : sig
  type real = X0 | X1 | X16
  type t = Real of real

  val to_string : t -> string
end = struct
  type real = X0 | X1 | X16
  type t = Real of real

  let to_string reg =
    match reg with
    | Real r -> ( match r with X0 -> "x0" | X1 -> "x1" | X16 -> "x16")
end

module Operand : sig
  type t = Reg of Register.t | Const of int

  val to_string : t -> string
end = struct
  type t = Reg of Register.t | Const of int

  let to_string op =
    match op with
    | Reg r -> Register.to_string r
    | Const i -> "#" ^ Int.to_string i
end

type t =
  | Add of Register.t * Register.t * Operand.t
  | Mov of Register.t * Operand.t
  | Svc of Operand.t
  | Push of Register.t
  | Pop of Register.t
  | Raw of string
  | Label of string

let to_string instr =
  match instr with
  | Add (r1, r2, op) ->
      "add " ^ Register.to_string r1 ^ ", " ^ Register.to_string r2 ^ ", "
      ^ Operand.to_string op
  | Mov (r1, op) -> "mov " ^ Register.to_string r1 ^ ", " ^ Operand.to_string op
  | Svc op -> "svc " ^ Operand.to_string op
  | Push r -> "str " ^ Register.to_string r ^ ", [sp, #-16]!"
  | Pop r -> "ldr " ^ Register.to_string r ^ ", [sp], #16"
  | Raw s -> s
  | Label l -> l ^ ":"
