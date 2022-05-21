open Parsing
open Ast

module Expr : sig
  type t =
    | Literal of Span.t * Type.t * Literal.t
    | Unary of Span.t * Type.t * Operator.unary * t
    | Sizeof of Span.t * Type.t * Type.t
    | Binary of Span.t * Type.t * t * Operator.binary * t
    | Var of Span.t * Type.t * string
    | Call of Span.t * Type.t * string * t list
    | PostFix of Span.t * Type.t * t * Operator.postfix
    | FieldAccess of Span.t * Type.t * t * string
    | Initializer of Span.t * Type.t * string * t list
    | Cast of Span.t * Type.t * t * Type.t

  val get_type : t -> Type.t
end

module Stmt : sig
  type t =
    | Declaration of declaration
    | Assignment of Span.t * Expr.t * Expr.t
    | Expr of Span.t * Expr.t
    | Block of Span.t * t list
    | If of Span.t * Expr.t * t * t option
    | While of Span.t * Expr.t * t
    | For of Span.t * t * Expr.t * Expr.t * t
    | Return of Span.t * Expr.t option
    | Break of Span.t
    | Continue of Span.t

  and declaration = {
    span : Span.t;
    mut : Type.mutability;
    id : string;
    type_annotation : Type.t option;
    defn : Expr.t option;
  }
end

type typed_var = Span.t * string * Type.t

module Fn : sig
  type t = {
    span : Span.t;
    id : string;
    args : typed_var list;
    typ : Type.t;
    body : Stmt.t;
  }
end

module Struct : sig
  type t = Span.t * string * typed_var list
end

module TopLevelElement : sig
  type t = Fn of Fn.t | Struct of Struct.t
end

type translation_unit = TopLevelElement.t list [@@deriving sexp, compare, equal]

val add_struct :
  (string, (string * Type.t) list, 'a) Base.Map.t ->
  Parsed_ast.Struct.t ->
  (string, (string * Type.t) list, 'a) Base.Map.t

val get_struct_map :
  TopLevelElement.t list ->
  (string, (string * Type.t) list, Base.String.comparator_witness) Base.Map.t
