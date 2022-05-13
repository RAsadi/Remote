open Ppx_compare_lib.Builtin
open Sexplib.Std

type literal = Int of int [@@deriving sexp, compare]
type unary_op = Neg | Bang | Tilde | Sizeof [@@deriving sexp, compare]

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
[@@deriving sexp, compare]

type postfix_op = Incr | Decr [@@deriving sexp, compare]
type _type = U32 [@@deriving sexp, compare]
type identifier = string [@@deriving sexp, compare]

type expr =
  | Literal of literal
  | Unary of unary_expr
  | Sizeof of identifier
  | Binary of binary_expr
  | Var of identifier
  | Conditional of expr * expr * expr
  | Call of identifier * expr list
  | PostFix of expr * postfix_op

and unary_expr = unary_op * expr
and binary_expr = expr * binary_op * expr
and var_expr = identifier [@@deriving sexp, compare]

type stmt =
  | Assignment of {
      is_mut : bool;
      id : identifier;
      type_annotation : _type option;
      defn : expr option;
    }
  | Expr of expr
  | Block of stmt list
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | Do of stmt * expr
  | For of identifier * expr * stmt
  | Return of expr option
  | Break
  | Continue
[@@deriving sexp, compare]

type typed_var = identifier * _type [@@deriving sexp, compare]

type fn = identifier * typed_var list * _type option * stmt
[@@deriving sexp, compare]

type top_level_element = Fn of fn [@@deriving sexp, compare]
type translation_unit = top_level_element list [@@deriving sexp, compare]