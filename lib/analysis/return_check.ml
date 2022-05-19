open Typing.Typed_ast
open Ast
open Base
open Base.Result.Let_syntax

let rec check_stmt (stmt : stmt) : bool =
  (* TODO, we can have an extra check here for loops, which can return if they loop forever*)
  match stmt with
  | Assignment _ -> false
  | Declaration _ -> false
  | Expr _ -> false
  | If (_, _cond, if_true, if_false) -> (
      check_stmt if_true
      && match if_false with Some s -> check_stmt s | None -> true)
  | While _ -> false
  | For _ -> false
  | Block (_, stmts) ->
      List.fold stmts ~init:false ~f:(fun acc stmt -> acc || check_stmt stmt)
  | Return _ -> true
  | Break _ -> true
  | Continue _ -> true

let check_fn (fn : fn) : unit Or_error.t =
  if Type.equal fn.typ Void then Ok ()
  else
    match check_stmt fn.body with
    | true -> Ok ()
    | false ->
        Or_error.error_string
          "There is a code path which doesn't return in this function"

let check_translation_unit trans : translation_unit Or_error.t =
  let%map _ =
    List.fold trans ~init:(Ok ()) ~f:(fun acc tl ->
        let%bind _ = acc in
        match tl with Fn fn -> check_fn fn | Struct _ -> Ok ())
  in
  trans
