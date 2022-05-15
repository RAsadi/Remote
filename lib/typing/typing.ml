open Ast
open Ast.Ast_types
open Parsing
open Base
open Base.Result.Let_syntax
open Typed_ast

type var_mapping = (string, _type * mutability, String.comparator_witness) Map.t
type fn_type_info = { ret : _type; arg_types : _type list }
type fn_mapping = (string, fn_type_info, String.comparator_witness) Map.t
type type_mapping = var_mapping * fn_mapping

type ctx = {
  var_map : var_mapping;
  fn_map : fn_mapping;
  curr_scope : var_mapping;
}

let lookup_var (ctx : ctx) id : (_type * mutability) Or_error.t =
  match Map.find ctx.curr_scope id with
  | None -> (
      match Map.find ctx.var_map id with
      | None ->
          Or_error.error_string
            ("cannot assign to " ^ id ^ " without first declaring")
      | Some a -> Ok a)
  | Some a -> Ok a

let lookup_fn (ctx : ctx) id : fn_type_info Or_error.t =
  match Map.find ctx.fn_map id with
  | None -> Or_error.error_string ("Couldn't find function " ^ id)
  | Some a -> Ok a

let rec type_expr (ctx : ctx) (expr : Parsed_ast.expr) :
    (_type * expr) Or_error.t =
  let type_expr = type_expr ctx in
  match expr with
  | Literal (span, l) -> (
      match l with
      | U32 i -> Ok (U32, Literal (span, U32, U32 i))
      | Bool b -> Ok (Bool, Literal (span, Bool, Bool b)))
  | Unary (span, op, e) -> type_unary_expr ctx span op e
  | Binary (span, e1, op, e2) -> type_binary_expr ctx span e1 op e2
  | Var (span, id) ->
      let%map var_type, _ = lookup_var ctx id in
      (var_type, Var (span, var_type, id))
  | Call (span, id, args) ->
      let%bind { ret; arg_types } = lookup_fn ctx id in
      (* First, check arg len *)
      if List.length args <> List.length arg_types then
        Or_error.error_string
          ("Calling " ^ id ^ " with incorrect number of args ")
      else
        let%bind arg_list =
          List.map args ~f:(fun x -> type_expr x) |> Result.all
        in
        let arg_type_list, args = List.unzip arg_list in
        (* make sure all the types line up*)
        if List.equal equal__type arg_type_list arg_types then
          Ok (ret, Call (span, ret, id, args))
        else Or_error.error_string "Arg types don't match"
  | Sizeof _ -> Or_error.error_string "TODO"
  | PostFix _ -> Or_error.error_string "TODO"

and type_binary_expr ctx span e1 op e2 =
  let%bind _type1, e1 = type_expr ctx e1 and _type2, e2 = type_expr ctx e2 in
  let%map new_type =
    match (op, _type1, _type2) with
    | Plus, Ast_types.U32, Ast_types.U32 -> Ok Ast_types.U32
    | Minus, U32, U32 -> Ok U32
    | Star, U32, U32 -> Ok U32
    | Slash, U32, U32 -> Ok U32
    (* Bitwise *)
    | LShift, U32, U32 -> Ok U32
    | RShift, U32, U32 -> Ok U32
    | And, U32, U32 -> Ok U32
    | Or, U32, U32 -> Ok U32
    | Xor, U32, U32 -> Ok U32
    | LAnd, Bool, Bool -> Ok Bool
    | LOr, Bool, Bool -> Ok Bool
    (* Equality *)
    | Eq, Bool, Bool -> Ok Bool
    | Neq, Bool, Bool -> Ok Bool
    | Eq, U32, U32 -> Ok Bool
    | Neq, U32, U32 -> Ok Bool
    (* Comparison *)
    | Lt, Bool, Bool -> Ok Bool
    | Lte, Bool, Bool -> Ok Bool
    | Gt, Bool, Bool -> Ok Bool
    | Gte, Bool, Bool -> Ok Bool
    | Lt, U32, U32 -> Ok Bool
    | Lte, U32, U32 -> Ok Bool
    | Gt, U32, U32 -> Ok Bool
    | Gte, U32, U32 -> Ok Bool
    | _ -> Or_error.error_string "TODO"
  in
  (new_type, Binary (span, new_type, e1, op, e2))

and type_unary_expr ctx span op e =
  let%bind _type, e = type_expr ctx e in
  let%map new_type =
    match _type with
    | Bool -> (
        match op with
        | Bang -> Ok Ast_types.Bool
        | _ -> Or_error.error_string "Cannot apply op to bool")
    | U32 -> (
        match op with
        | Tilde -> Ok U32
        | _ -> Or_error.error_string "Cannot apply op to u32")
    | Void -> Or_error.error_string "Cannot apply unary op to void"
  in
  (new_type, Unary (span, new_type, op, e))

(* TODO we should consider returning an annotated ast here, its not needed for now tho *)
let rec type_stmt ret_type (ctx : ctx) (stmt : Parsed_ast.stmt) : ctx Or_error.t
    =
  let type_stmt = type_stmt ret_type in
  match stmt with
  | Block (_, stmts) ->
      List.fold stmts
        ~init:
          (Ok
             {
               fn_map = ctx.fn_map;
               var_map = ctx.var_map;
               curr_scope = Map.empty (module String);
             })
        ~f:(fun acc stmt ->
          let%bind acc = acc in
          type_stmt acc stmt)
  | Expr (_, e) ->
      (* TODO there must be a better way*)
      let%map _ = type_expr ctx e in
      ctx
  | If (_, cond, if_true, if_false) -> (
      (* TODO there must be a better way*)
      let%bind _ = type_expr ctx cond in
      (* TODO check that the type for expr is bool, or convertable to bool *)
      let%bind _ = type_stmt ctx if_true in
      match if_false with
      | Some if_false ->
          let%map _ = type_stmt ctx if_false in
          ctx
      | None -> Ok ctx)
  | While (_, cond, body) ->
      let%bind _ = type_expr ctx cond in
      let%map _ = type_stmt ctx body in
      ctx
  | Return (_, e) -> (
      match e with
      | None -> (
          match ret_type with
          | Void -> Ok ctx
          | _ -> Or_error.error_string "Invalid return from void function")
      | Some e ->
          let%bind _type, _ = type_expr ctx e in
          if equal__type _type ret_type then Ok ctx
          else Or_error.error_string "Invalid return type")
  | Break _ -> Ok ctx
  | Continue _ -> Ok ctx
  | Declaration { span = _; mut; id; type_annotation; defn } -> (
      let%bind _ =
        match Map.find ctx.curr_scope id with
        | Some _ ->
            Or_error.error_string ("Trying to declare " ^ id ^ " multiple times")
        | None -> Ok ()
      in
      match defn with
      | Some defn ->
          (* If we have a defn, we should make sure the expr matches it *)
          let%bind expr_type, _ = type_expr ctx defn in
          let var_map = Map.set ctx.var_map ~key:id ~data:(expr_type, mut) in
          let curr_scope =
            Map.set ctx.curr_scope ~key:id ~data:(expr_type, mut)
          in
          let%map _ =
            match type_annotation with
            | None -> Ok ()
            | Some annotation ->
                if equal__type annotation expr_type then Ok ()
                else
                  Or_error.error_string
                    "Type annotation doesn't match definition"
          in
          { var_map; curr_scope; fn_map = ctx.fn_map }
      | None -> (
          (* If we don't have any definition, we need a type annotation*)
          let%bind _ =
            match mut with
            | Mut -> Ok ()
            | Const ->
                Or_error.error_string "Cannot have a const without a definition"
          in
          match type_annotation with
          | None -> Or_error.error_string "Type annotation required"
          | Some annotation ->
              let var_map =
                Map.set ctx.var_map ~key:id ~data:(annotation, mut)
              in
              let curr_scope =
                Map.set ctx.curr_scope ~key:id ~data:(annotation, mut)
              in
              Ok { var_map; curr_scope; fn_map = ctx.fn_map }))
  | Assignment (_, id, expr) ->
      let%bind _type, _ = type_expr ctx expr in
      let%bind expected, mut = lookup_var ctx id in
      if equal__type _type expected && equal_mutability mut Mut then Ok ctx
      else Or_error.error_string "Invalid assignment"
  | For _ -> Or_error.error_string "TODO"

let type_fn fn_map (fn : Parsed_ast.fn) =
  let var_map =
    List.fold fn.args
      ~init:(Map.empty (module String))
      ~f:(fun acc arg ->
        let _, id, _type = arg in
        Map.set acc ~key:id ~data:(_type, Const))
  in
  let ctx = { var_map; fn_map; curr_scope = Map.empty (module String) } in
  let%map _ = type_stmt fn._type ctx fn.body in
  ()

let get_fn_sig fn_map (fn : Parsed_ast.fn) =
  let arg_types = List.map fn.args ~f:(fun (_, _, _type) -> _type) in
  Map.set fn_map ~key:fn.id ~data:{ ret = fn._type; arg_types }

let type_translation_unit (translation_unit : Parsed_ast.translation_unit) =
  let fn_map : fn_mapping =
    List.fold translation_unit
      ~init:(Map.empty (module String))
      ~f:(fun acc arg -> match arg with Fn arg -> get_fn_sig acc arg)
  in
  let%map _ =
    List.map translation_unit ~f:(fun tl ->
        match tl with Fn fn -> type_fn fn_map fn)
    |> Result.all
  in
  translation_unit
