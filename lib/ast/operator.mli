type unary = Neg | Bang | Tilde | Addr [@@deriving sexp, compare, equal, show]

type binary =
  (* Arithmetic *)
  | Plus
  | Minus
  | Star
  | Slash
  | Mod
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
[@@deriving sexp, compare, equal, show]

type postfix = Incr | Decr | Deref [@@deriving sexp, compare, equal, show]