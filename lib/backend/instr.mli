module Register : sig
  type t = X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X16 | X30 | Fp | Sp

  val to_string : t -> string
  val from_int : int -> t
end

module Operand : sig
  type t = Reg of Register.t | Const of int | Label of string

  val to_string : t -> string
end

type label = string

type t =
  | Add of Register.t * Register.t * Operand.t
  | Sub of Register.t * Register.t * Operand.t
  | Mul of Register.t * Register.t * Operand.t
  | Div of Register.t * Register.t * Operand.t
  | MSub of Register.t * Register.t * Register.t * Register.t
  | Lsl of Register.t * Register.t * Operand.t
  | Lsr of Register.t * Register.t * Operand.t
  | Eor of Register.t * Register.t * Operand.t
  | Orr of Register.t * Register.t * Operand.t
  | And of Register.t * Register.t * Operand.t
  | Mov of Register.t * Operand.t
  | Adr of Register.t * string
  | Cmp of Register.t * Operand.t
  | Beq of label
  | Bne of label
  | B of label
  | Bl of label
  | Svc of Operand.t
  | CSet of Register.t * label
  | Neg of Register.t * Operand.t
  | Mvn of Register.t * Operand.t
  | Push of Register.t
  | Pop of Register.t
  | Ret
  | Raw of label
  | Label of label

val to_string : t -> string
