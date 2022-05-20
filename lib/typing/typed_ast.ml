open Ast
open Parsing
open Base

(* TODO, could just make these generic instead of copying this over from the parsed_ast.*)
module Expr : sig
  type t =
    | Literal of Span.t * Type.t * Literal.t
    | Unary of Span.t * Type.t * Operator.unary * t
    | Sizeof of Span.t * Type.t * Type.t
    | Binary of Span.t * Type.t * t * Operator.binary * t
    | Var of Span.t * Type.t * Identifier.t
    | Call of Span.t * Type.t * Identifier.t * t list
    | PostFix of Span.t * Type.t * t * Operator.postfix
    | FieldAccess of Span.t * Type.t * t * Identifier.t
    | Initializer of Span.t * Type.t * Identifier.t * t list
  [@@deriving sexp, compare, equal]

  val get_type : t -> Type.t
end = struct
  type t =
    | Literal of Span.t * Type.t * Literal.t
    | Unary of Span.t * Type.t * Operator.unary * t
    | Sizeof of Span.t * Type.t * Type.t
    | Binary of Span.t * Type.t * t * Operator.binary * t
    | Var of Span.t * Type.t * Identifier.t
    | Call of Span.t * Type.t * Identifier.t * t list
    | PostFix of Span.t * Type.t * t * Operator.postfix
    | FieldAccess of Span.t * Type.t * t * Identifier.t
    | Initializer of Span.t * Type.t * Identifier.t * t list
  [@@deriving sexp, compare, equal]

  let get_type expr =
    match expr with
    | Literal (_, typ, _) -> typ
    | Unary (_, typ, _, _) -> typ
    | Sizeof (_, typ, _) -> typ
    | Binary (_, typ, _, _, _) -> typ
    | Var (_, typ, _) -> typ
    | Call (_, typ, _, _) -> typ
    | PostFix (_, typ, _, _) -> typ
    | FieldAccess (_, typ, _, _) -> typ
    | Initializer (_, typ, _, _) -> typ
end

module Stmt : sig
  type t =
    | Declaration of declaration
    | Assignment of Span.t * Expr.t * Expr.t
    | Expr of Span.t * Expr.t
    | Block of Span.t * t list
    | If of Span.t * Expr.t * t * t option
    | While of Span.t * Expr.t * t
    | For of Span.t * Identifier.t * Expr.t * t
    | Return of Span.t * Expr.t option
    | Break of Span.t
    | Continue of Span.t

  and declaration = {
    span : Span.t;
    mut : Type.mutability;
    id : Identifier.t;
    type_annotation : Type.t option;
    defn : Expr.t option;
  }
  [@@deriving sexp, compare, equal]
end = struct
  type t =
    | Declaration of declaration
    | Assignment of Span.t * Expr.t * Expr.t
    | Expr of Span.t * Expr.t
    | Block of Span.t * t list
    | If of Span.t * Expr.t * t * t option
    | While of Span.t * Expr.t * t
    | For of Span.t * Identifier.t * Expr.t * t
    | Return of Span.t * Expr.t option
    | Break of Span.t
    | Continue of Span.t

  and declaration = {
    span : Span.t;
    mut : Type.mutability;
    id : Identifier.t;
    type_annotation : Type.t option;
    defn : Expr.t option;
  }
  [@@deriving sexp, compare, equal]
end

type typed_var = Span.t * Identifier.t * Type.t
[@@deriving sexp, compare, equal]

module Fn : sig
  type t = {
    span : Span.t;
    id : Identifier.t;
    args : typed_var list;
    typ : Type.t;
    body : Stmt.t;
  }
  [@@deriving sexp, compare, equal]
end = struct
  type t = {
    span : Span.t;
    id : Identifier.t;
    args : typed_var list;
    typ : Type.t;
    body : Stmt.t;
  }
  [@@deriving sexp, compare, equal]
end

module Struct : sig
  type t = Span.t * Identifier.t * typed_var list
  [@@deriving sexp, compare, equal]
end = struct
  type t = Span.t * Identifier.t * typed_var list
  [@@deriving sexp, compare, equal]
end

module TopLevelElement : sig
  type t = Fn of Fn.t | Struct of Struct.t [@@deriving sexp, compare, equal]
end = struct
  type t = Fn of Fn.t | Struct of Struct.t [@@deriving sexp, compare, equal]
end

type translation_unit = TopLevelElement.t list [@@deriving sexp, compare, equal]

let add_struct struct_map ((_, id, var_list) : Parsed_ast.Struct.t) =
  let var_list = List.map var_list ~f:(fun (_, id, typ) -> (id, typ)) in
  Map.set struct_map ~key:id ~data:var_list

let get_struct_map translation_unit =
  List.fold translation_unit
    ~init:(Map.empty (module String))
    ~f:(fun acc arg ->
      match arg with
      | TopLevelElement.Fn _ -> acc
      | Struct s -> add_struct acc s)
