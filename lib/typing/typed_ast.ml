open Ppx_compare_lib.Builtin
open Sexplib.Std
open Ast.Ast_types
open Parsing

(* TODO, really should just make these generic
   instead of copying this over from the parsed_ast.
*)

type identifier = string [@@deriving sexp, compare, equal]

type expr =
  | Literal of Span.t * _type * literal
  | Unary of unary_expr
  | Sizeof of Span.t * _type * _type
  | Binary of binary_expr
  | Var of Span.t * _type * identifier
  | Call of Span.t * _type * identifier * expr list
  | PostFix of Span.t * _type * expr * postfix_op
  | FieldAccess of Span.t * _type * expr * identifier
  | Initializer of Span.t * _type * identifier * expr list

and unary_expr = Span.t * _type * unary_op * expr

and binary_expr = Span.t * _type * expr * binary_op * expr
[@@deriving sexp, compare, equal]

let get_expr_type (expr : expr) =
  match expr with
  | Literal (_, _type, _) -> _type
  | Unary (_, _type, _, _) -> _type
  | Sizeof (_, _type, _) -> _type
  | Binary (_, _type, _, _, _) -> _type
  | Var (_, _type, _) -> _type
  | Call (_, _type, _, _) -> _type
  | PostFix (_, _type, _, _) -> _type
  | FieldAccess (_, _type, _, _) -> _type
  | Initializer (_, _type, _, _) -> _type

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