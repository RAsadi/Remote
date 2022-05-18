open Ast
open Ast.Ast_types
open Parsing
open Base
open Base.Result.Let_syntax
open Typed_ast

type var_mapping = (string, _type * mutability, String.comparator_witness) Map.t
type fn_type_info = { ret : _type; arg_types : _type list }
type fn_mapping = (string, fn_type_info, String.comparator_witness) Map.t
type struct_type_info = (identifier * _type) list

type struct_mapping =
  (string, struct_type_info, String.comparator_witness) Map.t

type ctx = {
  var_map : var_mapping;
  struct_map : struct_mapping;
  fn_map : fn_mapping;
  curr_scope : var_mapping;
}

let lookup_var (ctx : ctx) id : (_type * mutability) Or_error.t =
  match Map.find ctx.curr_scope id with
  | None -> (
      match Map.find ctx.var_map id with
      | None ->
          Or_error.error_string
            ("cannot lookup " ^ id ^ " without first declaring")
      | Some a -> Ok a)
  | Some a -> Ok a

let lookup_fn (ctx : ctx) id : fn_type_info Or_error.t =
  match Map.find ctx.fn_map id with
  | None -> Or_error.error_string ("Couldn't find function " ^ id)
  | Some a -> Ok a

let lookup_struct ctx id =
  match Map.find ctx.struct_map id with
  | None -> Or_error.error_string ("Couldn't find struct " ^ id)
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
  | PostFix (span, e, op) -> type_postfix_expr ctx span e op
  | FieldAccess (span, expr, id) -> type_field_access ctx span expr id
  | Initializer (span, id, inits) -> type_initializer ctx span id inits

and type_initializer ctx span id inits =
  let%bind struct_init_types = lookup_struct ctx id in
  (* First, check arg len *)
  if List.length inits <> List.length struct_init_types then
    Or_error.error_string
      ("Trying to initialize " ^ id ^ " with incorrect number of args ")
  else
    let%bind init_list =
      List.map inits ~f:(fun x -> type_expr ctx x) |> Result.all
    in
    let struct_init_types =
      List.map struct_init_types ~f:(fun (_, _type) -> _type)
    in
    let inits_type_list, inits = List.unzip init_list in
    (* make sure all the types line up*)
    if List.equal equal__type inits_type_list struct_init_types then
      Ok (Ast_types.Struct id, Initializer (span, Struct id, id, inits))
    else Or_error.error_string "Arg types don't match"

and type_field_access ctx span expr id =
  let%bind _type, expr = type_expr ctx expr in
  match _type with
  | Struct struct_id -> (
      let%bind var_list = lookup_struct ctx struct_id in
      match
        List.find var_list ~f:(fun (var_id, _) -> equal_identifier id var_id)
      with
      | None -> Or_error.error_string "Could not find matching member in struct"
      | Some (_, var_type) ->
          Ok (var_type, FieldAccess (span, var_type, expr, id)))
  | _ -> Or_error.error_string "Cannot access field on non struct type"

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
    match op with
    | Addr -> (
        match e with
        | Var (_, t, _) -> Ok (Pointer t)
        | _ -> Or_error.error_string "Cannot take addr of temporary")
    | _ -> (
        match _type with
        | Bool -> (
            match op with
            | Bang -> Ok Ast_types.Bool
            | _ -> Or_error.error_string "Cannot apply unary op to bool")
        | U32 -> (
            match op with
            | Tilde -> Ok U32
            | _ -> Or_error.error_string "Cannot apply unary op to u32")
        | Void -> Or_error.error_string "Cannot apply unary op to void"
        | Struct i -> (
            match op with
            | Addr -> Ok (Pointer (Struct i))
            | _ -> Or_error.error_string "Cannot apply unary op to struct")
        | Pointer _ -> Or_error.error_string "Cannot apply unary op to ptr")
  in

  (new_type, Unary (span, new_type, op, e))

and type_postfix_expr ctx span e op =
  let%bind _type, e = type_expr ctx e in
  let%map new_type =
    match _type with
    | Bool -> Or_error.error_string "Cannot apply postfix op to bool"
    | U32 -> (
        match op with
        | Incr | Decr -> Ok Ast_types.U32
        | _ -> Or_error.error_string "Cannot apply postfix op to u32")
    | Void -> Or_error.error_string "Cannot apply postfix op to void"
    | Struct _ -> Or_error.error_string "Cannot apply postfix op to struct"
    | Pointer s -> (
        match op with
        | Deref -> Ok s
        | _ -> Or_error.error_string "Cannot apply postfix op to ptr")
  in
  (new_type, PostFix (span, new_type, e, op))

(* TODO we should consider returning an annotated ast here, its not needed for now tho *)
let rec type_stmt ret_type (ctx : ctx) (stmt : Parsed_ast.stmt) :
    (ctx * stmt) Or_error.t =
  let type_stmt = type_stmt ret_type in
  match stmt with
  | Block (span, stmts) ->
      let%map new_ctx, stmt_list =
        List.fold stmts
          ~init:
            (Ok
               ( {
                   fn_map = ctx.fn_map;
                   var_map = ctx.var_map;
                   struct_map = ctx.struct_map;
                   curr_scope = Map.empty (module String);
                 },
                 [] ))
          ~f:(fun acc stmt ->
            let%bind ctx, rest = acc in
            let%map new_ctx, new_stmt = type_stmt ctx stmt in
            (new_ctx, rest @ [ new_stmt ]))
      in
      (new_ctx, Block (span, stmt_list))
  | Expr (span, e) ->
      (* TODO there must be a better way*)
      let%map _, e = type_expr ctx e in
      (ctx, Expr (span, e))
  | If (span, cond, if_true, if_false) -> (
      (* TODO there must be a better way*)
      let%bind _, typed_cond = type_expr ctx cond in
      (* TODO check that the type for expr is bool, or convertable to bool *)
      let%bind _, typed_if_true = type_stmt ctx if_true in
      match if_false with
      | Some if_false ->
          let%map _, typed_if_false = type_stmt ctx if_false in
          (ctx, If (span, typed_cond, typed_if_true, Some typed_if_false))
      | None -> Ok (ctx, If (span, typed_cond, typed_if_true, None)))
  | While (span, cond, body) ->
      let%bind _, typed_cond = type_expr ctx cond in
      let%map _, typed_body = type_stmt ctx body in
      (ctx, While (span, typed_cond, typed_body))
  | Return (span, e) -> (
      match e with
      | None -> (
          match ret_type with
          | Void -> Ok (ctx, Return (span, None))
          | _ -> Or_error.error_string "Invalid return from void function")
      | Some e ->
          let%bind _type, typed_e = type_expr ctx e in
          if equal__type _type ret_type then
            Ok (ctx, Return (span, Some typed_e))
          else Or_error.error_string "Invalid return type")
  | Break span -> Ok (ctx, Break span)
  | Continue span -> Ok (ctx, Continue span)
  | Declaration { span; mut; id; type_annotation; defn } -> (
      let%bind _ =
        match Map.find ctx.curr_scope id with
        | Some _ ->
            Or_error.error_string ("Trying to declare " ^ id ^ " multiple times")
        | None -> Ok ()
      in
      match defn with
      | Some defn ->
          (* If we have a defn, we should make sure the expr matches it *)
          let%bind expr_type, typed_defn = type_expr ctx defn in
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
          ( {
              var_map;
              curr_scope;
              fn_map = ctx.fn_map;
              struct_map = ctx.struct_map;
            },
            Declaration
              { span; mut; id; type_annotation; defn = Some typed_defn } )
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
              Ok
                ( {
                    var_map;
                    curr_scope;
                    fn_map = ctx.fn_map;
                    struct_map = ctx.struct_map;
                  },
                  Declaration { span; mut; id; type_annotation; defn = None } ))
      )
  | Assignment (span, id, expr) ->
      (* The LHS of all assignments must either be an ID, or have a pointer type *)
      let%bind _type, typed_expr = type_expr ctx expr in
      let%bind expected, mut = lookup_var ctx id in
      if equal__type _type expected && equal_mutability mut Mut then
        Ok (ctx, Assignment (span, id, typed_expr))
      else Or_error.error_string "Invalid assignment"
  | For _ -> Or_error.error_string "TODO"

let type_fn struct_map fn_map (fn : Parsed_ast.fn) =
  let var_map =
    List.fold fn.args
      ~init:(Map.empty (module String))
      ~f:(fun acc arg ->
        let _, id, _type = arg in
        Map.set acc ~key:id ~data:(_type, Const))
  in
  let ctx =
    { struct_map; var_map; fn_map; curr_scope = Map.empty (module String) }
  in
  let%map _, body = type_stmt fn._type ctx fn.body in
  { span = fn.span; id = fn.id; args = fn.args; _type = fn._type; body }

let get_fn_sig fn_map (fn : Parsed_ast.fn) =
  let arg_types = List.map fn.args ~f:(fun (_, _, _type) -> _type) in
  Map.set fn_map ~key:fn.id ~data:{ ret = fn._type; arg_types }

let add_struct struct_map ((_, id, var_list) : Parsed_ast._struct) =
  let var_list = List.map var_list ~f:(fun (_, id, _type) -> (id, _type)) in
  Map.set struct_map ~key:id ~data:var_list

let get_struct_map translation_unit =
  List.fold translation_unit
    ~init:(Map.empty (module String))
    ~f:(fun acc arg ->
      match arg with Fn _ -> acc | Struct s -> add_struct acc s)

let type_translation_unit (translation_unit : Parsed_ast.translation_unit) =
  (* functions we get from the stdlib *)
  let init_fn_map =
    Map.of_alist_exn
      (module String)
      [ ("__malloc", { ret = U32; arg_types = [ U32 ] }) ]
  in
  let fn_map : fn_mapping =
    List.fold translation_unit ~init:init_fn_map ~f:(fun acc arg ->
        match arg with Fn arg -> get_fn_sig acc arg | Struct _ -> acc)
  in
  let struct_map =
    List.fold translation_unit
      ~init:(Map.empty (module String))
      ~f:(fun acc arg ->
        match arg with Fn _ -> acc | Struct s -> add_struct acc s)
  in
  let%map new_trans =
    List.map translation_unit ~f:(fun tl ->
        match tl with
        | Fn fn ->
            let%map fn = type_fn struct_map fn_map fn in
            Fn fn
        | Struct s -> Ok (Struct s))
    |> Result.all
  in
  new_trans
