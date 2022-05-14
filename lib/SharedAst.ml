type _type = U32 | Bool | Void [@@deriving sexp, compare, equal]
type mutability = Mut | Const [@@deriving sexp, compare, equal]
type unary_op = Neg | Bang | Tilde [@@deriving sexp, compare, equal]

type binary_op =
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

type postfix_op = Incr | Decr [@@deriving sexp, compare, equal]
