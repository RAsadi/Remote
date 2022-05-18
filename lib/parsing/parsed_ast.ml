open Ppx_compare_lib.Builtin
open Sexplib.Std
open Ast.Ast_types

type expr =
  | Literal of Span.t * literal
  | Unary of unary_expr
  | Sizeof of Span.t * identifier
  | Binary of binary_expr
  | Var of Span.t * identifier
  | Call of Span.t * identifier * expr list
  | PostFix of Span.t * expr * postfix_op
  | FieldAccess of Span.t * expr * identifier
  | Initializer of Span.t * identifier * expr list

and unary_expr = Span.t * unary_op * expr

and binary_expr = Span.t * expr * binary_op * expr
[@@deriving sexp, compare, equal]

type stmt =
  | Declaration of declaration
  | Assignment of Span.t * expr * expr
  | Expr of Span.t * expr
  | Block of Span.t * stmt list
  | If of Span.t * expr * stmt * stmt option
  | While of Span.t * expr * stmt
  | For of Span.t * identifier * expr * stmt
  | Return of Span.t * expr option
  | Break of Span.t
  | Continue of Span.t

and declaration = {
  span : Span.t;
  mut : mutability;
  id : identifier;
  type_annotation : _type option;
  defn : expr option;
}
[@@deriving sexp, compare, equal]

type typed_var = Span.t * identifier * _type [@@deriving sexp, compare, equal]

type fn = {
  span : Span.t;
  id : identifier;
  args : typed_var list;
  _type : _type;
  body : stmt;
}
[@@deriving sexp, compare, equal]

type _struct = Span.t * identifier * typed_var list
[@@deriving sexp, compare, equal]

type top_level_element = Fn of fn | Struct of _struct
[@@deriving sexp, compare, equal]

type translation_unit = top_level_element list [@@deriving sexp, compare, equal]