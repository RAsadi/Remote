module Register : sig
  type t = X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X16 | X30 | Fp | Sp

  val to_string : t -> string
  val from_int : int -> t
end = struct
  type t = X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X16 | X30 | Fp | Sp

  let to_string reg =
    match reg with
    | X0 -> "x0"
    | X1 -> "x1"
    | X2 -> "x2"
    | X3 -> "x3"
    | X4 -> "x4"
    | X5 -> "x5"
    | X6 -> "x6"
    | X7 -> "x7"
    | X16 -> "x16"
    | X30 -> "x30"
    | Fp -> "fp"
    | Sp -> "sp"

  let from_int i =
    assert (i < 30);
    match i with
    | 0 -> X0
    | 1 -> X1
    | 2 -> X2
    | 3 -> X3
    | 4 -> X4
    | 5 -> X5
    | 6 -> X6
    | 7 -> X7
    | 29 -> Fp
    | 30 -> X30
    | _ -> Sp (* lol *)
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
  | Bl of label
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
  | Bl label -> "bl " ^ label
  | Ret -> "ret"
  | CSet (r, op) -> "cset " ^ Register.to_string r ^ ", " ^ op
  | Svc op -> "svc " ^ Operand.to_string op
  | Push r -> "str " ^ Register.to_string r ^ ", [sp, #-16]!"
  | Pop r -> "ldr " ^ Register.to_string r ^ ", [sp], #16"
  | Raw s -> s
  | Label l -> l ^ ":"
