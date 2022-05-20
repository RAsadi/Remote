type unary = Neg | Bang | Tilde | Addr [@@deriving sexp, compare, equal]

type binary =
  (* Arithmetic *)
  | Plus
  | Minus
  | Star
  | Slash
  (* Bitwise *)
  | LShift
  | RShift
  | And
  | Or
  | Xor
  | LAnd
  | LOr
  (* Equality *)
  | Eq
  | Neq
  (* Comparison *)
  | Lt
  | Lte
  | Gt
  | Gte
[@@deriving sexp, compare, equal]

type postfix = Incr | Decr | Deref [@@deriving sexp, compare, equal]