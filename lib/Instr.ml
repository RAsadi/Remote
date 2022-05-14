module Register : sig
  type real = X0 | X1 | X16 | X29 | Sp
  type t = Real of real

  val to_string : t -> string
end = struct
  type real = X0 | X1 | X16 | X29 | Sp
  type t = Real of real

  let to_string reg =
    match reg with
    | Real r -> (
        match r with
        | X0 -> "x0"
        | X1 -> "x1"
        | X16 -> "x16"
        | X29 -> "x29"
        | Sp -> "sp")
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

type label = string

type t =
  | Add of Register.t * Register.t * Operand.t
  | Sub of Register.t * Register.t * Operand.t
  | Mul of Register.t * Register.t * Operand.t
  | Div of Register.t * Register.t * Operand.t
  | Lsl of Register.t * Register.t * Operand.t
  | Lsr of Register.t * Register.t * Operand.t
  | Eor of Register.t * Register.t * Operand.t
  | Orr of Register.t * Register.t * Operand.t
  | And of Register.t * Register.t * Operand.t
  | Mov of Register.t * Operand.t
  | Cmp of Register.t * Operand.t
  | Beq of label
  | Bne of label
  | B of label
  | Svc of Operand.t
  | CSet of Register.t * string
  | Neg of Register.t * Operand.t
  | Mvn of Register.t * Operand.t
  | Push of Register.t
  | Pop of Register.t
  | Ret
  | Raw of string
  | Label of label

let to_string instr =
  match instr with
  | Add (r1, r2, op) ->
      "add " ^ Register.to_string r1 ^ ", " ^ Register.to_string r2 ^ ", "
      ^ Operand.to_string op
  | Sub (r1, r2, op) ->
      "sub " ^ Register.to_string r1 ^ ", " ^ Register.to_string r2 ^ ", "
      ^ Operand.to_string op
  | Mul (r1, r2, op) ->
      "mul " ^ Register.to_string r1 ^ ", " ^ Register.to_string r2 ^ ", "
      ^ Operand.to_string op
  | Div (r1, r2, op) ->
      "sdiv " ^ Register.to_string r1 ^ ", " ^ Register.to_string r2 ^ ", "
      ^ Operand.to_string op
  | Lsl (r1, r2, op) ->
      "lsl " ^ Register.to_string r1 ^ ", " ^ Register.to_string r2 ^ ", "
      ^ Operand.to_string op
  | Lsr (r1, r2, op) ->
      "lsr " ^ Register.to_string r1 ^ ", " ^ Register.to_string r2 ^ ", "
      ^ Operand.to_string op
  | Eor (r1, r2, op) ->
      "eor " ^ Register.to_string r1 ^ ", " ^ Register.to_string r2 ^ ", "
      ^ Operand.to_string op
  | Orr (r1, r2, op) ->
      "orr " ^ Register.to_string r1 ^ ", " ^ Register.to_string r2 ^ ", "
      ^ Operand.to_string op
  | And (r1, r2, op) ->
      "and " ^ Register.to_string r1 ^ ", " ^ Register.to_string r2 ^ ", "
      ^ Operand.to_string op
  | Mov (r1, op) -> "mov " ^ Register.to_string r1 ^ ", " ^ Operand.to_string op
  | Cmp (r1, op) -> "cmp " ^ Register.to_string r1 ^ ", " ^ Operand.to_string op
  | Mvn (r1, op) -> "mvn " ^ Register.to_string r1 ^ ", " ^ Operand.to_string op
  | Neg (r1, op) -> "neg " ^ Register.to_string r1 ^ ", " ^ Operand.to_string op
  | Beq label -> "beq " ^ label
  | Bne label -> "bne " ^ label
  | B label -> "b " ^ label
  | Ret -> "ret"
  | CSet (r, op) -> "cset " ^ Register.to_string r ^ ", " ^ op
  | Svc op -> "svc " ^ Operand.to_string op
  | Push r -> "str " ^ Register.to_string r ^ ", [sp, #-16]!"
  | Pop r -> "ldr " ^ Register.to_string r ^ ", [sp], #16"
  | Raw s -> s
  | Label l -> l ^ ":"
