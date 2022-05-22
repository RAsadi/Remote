open Base
open Base.Result.Let_syntax
open Typing.Typed_ast
open Ast
open Ast.Type

type variable_info = { offset : int; typ : Type.t }
type variable_mapping = (string, variable_info, String.comparator_witness) Map.t
type struct_type_info = (string * Type.t) list

type struct_mapping =
  (string, struct_type_info, String.comparator_witness) Map.t

type exec_context = {
  var_map : variable_mapping;
  curr_scope : variable_mapping;
  struct_mapping : struct_mapping;
  continue_label : string option;
  break_label : string option;
}

let _print_map (var_map : variable_mapping) =
  Map.iter_keys var_map ~f:(fun k ->
      let { offset; _ } = Map.find_exn var_map k in
      Stdio.print_endline (k ^ ": " ^ Int.to_string offset))

let rec get_type_size ctx _type =
  match _type with
  | Any -> raise (Failure "uhoh")
  | U32 -> 4
  | U8 -> 1
  | Bool -> 1
  | Void -> 0
  | Pointer _ -> 8
  | Struct id ->
      let members = Map.find_exn ctx.struct_mapping id in
      List.fold members ~init:0 ~f:(fun acc (_, member) ->
          acc + get_type_size ctx member)

let get_offset_from_field ctx struct_type field_name =
  match struct_type with
  | Struct struct_id ->
      let struct_info = Map.find_exn ctx.struct_mapping struct_id in
      let _, space_needed =
        List.fold struct_info ~init:(false, 0)
          ~f:(fun (seen, total) (id, curr) ->
            if seen then (true, total)
            else
              let new_total = get_type_size ctx curr + total in
              if String.equal id field_name then (true, new_total)
              else (false, new_total))
      in
      space_needed
  | _ -> raise (Failure "unreachable")

(* Gives an offset in bytes *)
let lookup_var (ctx : exec_context) id : variable_info Or_error.t =
  match Map.find ctx.curr_scope id with
  | Some info -> Ok info
  | None -> (
      match Map.find ctx.var_map id with
      | Some info -> Ok info
      | None -> Or_error.error_string "unable to get info for var")

let lookup_struct (ctx : exec_context) id =
  match Map.find ctx.struct_mapping id with
  | Some info -> Ok info
  | None -> Or_error.error_string "unable to get info for struct"

let store_with_offset ctx _type start offset reg_num =
  let type_size = get_type_size ctx _type in
  let offset = Int.to_string offset in
  let rn = Int.to_string reg_num in
  match type_size with
  | 1 -> [ Instr.Raw ("strb w" ^ rn ^ ", [" ^ start ^ ", #-" ^ offset ^ "]") ]
  | 2 -> [ Instr.Raw ("strh x" ^ rn ^ ", [" ^ start ^ ", #-" ^ offset ^ "]") ]
  | 4 -> [ Instr.Raw ("str w" ^ rn ^ ", [" ^ start ^ ", #-" ^ offset ^ "]") ]
  | 8 -> [ Instr.Raw ("str x" ^ rn ^ ", [" ^ start ^ ", #-" ^ offset ^ "]") ]
  | _ -> raise (Failure "unreachable")

let load_from_offset ctx _type start offset reg_num =
  let type_size = get_type_size ctx _type in
  let offset = Int.to_string offset in
  let rn = Int.to_string reg_num in
  match type_size with
  | 1 -> [ Instr.Raw ("ldrb w" ^ rn ^ ", [" ^ start ^ ", #-" ^ offset ^ "]") ]
  | 2 -> [ Instr.Raw ("ldrh x" ^ rn ^ ", [" ^ start ^ ", #-" ^ offset ^ "]") ]
  | 4 -> [ Instr.Raw ("ldr w" ^ rn ^ ", [" ^ start ^ ", #-" ^ offset ^ "]") ]
  | 8 -> [ Instr.Raw ("ldr x" ^ rn ^ ", [" ^ start ^ ", #-" ^ offset ^ "]") ]
  | _ -> raise (Failure "unreachable")

(* Value will be loaded into x0 *)
let load_var ctx var_type var_id =
  let%map { offset; _ } = lookup_var ctx var_id in
  load_from_offset ctx var_type "fp" offset 0

(* Value to store should be in x0 *)
let store_var ctx var_type var_id =
  let%map { offset; _ } = lookup_var ctx var_id in
  store_with_offset ctx var_type "fp" offset 0

(* Returns a new, unique label *)
let label_count = ref 0

let gen_label label_name =
  Int.incr label_count;
  label_name ^ Int.to_string !label_count

(* We know it has to exist because of type checking *)

(* lol hack *)
let string_list = ref []

let rec gen_expr (ctx : exec_context) (expr : Expr.t) : Instr.t list Or_error.t
    =
  match expr with
  | Literal (_, _, l) -> (
      match l with
      | Num i -> Ok [ Instr.Mov (X0, Const i) ]
      | Bool b -> Ok [ Instr.Mov (X0, Const (Bool.to_int b)) ]
      | Char i -> Ok [ Instr.Mov (X0, Const (Char.to_int i)) ]
      | Null -> Ok [ Instr.Mov (X0, Const 0) ]
      | String s ->
          let str_label = gen_label "strlit" in
          string_list := (str_label, s) :: !string_list;
          Ok [ Instr.Adr (X0, str_label) ])
  | Unary (_, _, op, e) -> gen_unary ctx op e
  | Binary (_, _, e1, op, e2) -> gen_binary ctx e1 op e2
  | Var (_, _type, id) -> (
      match _type with
      | Struct _ ->
          let%map { offset; _ } = lookup_var ctx id in
          [ Instr.Raw ("sub x0, fp, #" ^ Int.to_string offset) ]
      | _ -> load_var ctx _type id)
  | Sizeof (_, _, sizeof_type) ->
      let%map size =
        match sizeof_type with
        | U32 | U8 | Bool | Void | Pointer _ | Any ->
            Ok (get_type_size ctx sizeof_type)
        | Struct id -> (
            let var_res = lookup_var ctx id in
            match var_res with
            | Ok { offset = _; typ } -> Ok (get_type_size ctx typ)
            | Error _ ->
                let%map _ = lookup_struct ctx id in
                get_type_size ctx (Struct id))
      in
      [ Instr.Mov (X0, Const size) ]
  | Call (_, _, id, args) ->
      (*
          TODO In the future, we should adhere strictly to arm64 ABI.
          For now, we will pass in args through x0-x7; and fail otherwise
      *)
      let num_args = List.length args in
      if num_args > 8 then
        Or_error.error_string
          "cannot handle function calls with more than 8 args"
      else
        let%map push_instrs =
          List.fold args ~init:(Ok []) ~f:(fun acc arg ->
              let%bind acc = acc in
              let%map expr_instrs = gen_expr ctx arg in
              acc @ expr_instrs @ [ Instr.Push X0 ])
        in
        let load_instrs =
          List.rev_mapi args ~f:(fun idx _ ->
              Instr.Pop (Instr.Register.from_int idx))
        in
        push_instrs @ load_instrs @ [ Instr.Bl id ]
  | PostFix (_, _type, e, op) -> gen_postfix ctx _type e op
  | FieldAccess (_, _type, lhs, field_name) ->
      let%bind struct_type =
        match lhs with
        | Var (_, t, _) -> Ok t
        | _ -> (
            let typ = Expr.get_type lhs in
            match typ with
            | Struct _ -> Ok typ
            | _ -> Or_error.error_string "TODO cannot handle non var lhs")
      in
      let%map lhs_instrs = gen_expr ctx lhs in
      (* Based on codegen for var, x0 should now hold the address of var *)
      let field_offset = get_offset_from_field ctx struct_type field_name in
      lhs_instrs @ load_from_offset ctx _type "x0" field_offset 0
  | Initializer (_, _type, struct_id, inits) ->
      (* Generate and push things onto the stack.
         This is technically broken in the case of returning from a function, but thats ok for now.
         Note that stack space is only reclaimed after the function exits, which is kinda scuffed
      *)
      let struct_info = Map.find_exn ctx.struct_mapping struct_id in
      let%map stack_move_amt, push_instrs =
        List.fold
          (List.zip_exn struct_info inits)
          ~init:(Ok (0, []))
          ~f:(fun acc (info, arg) ->
            let%bind curr_offset, acc = acc in
            let%map expr_instrs = gen_expr ctx arg in
            let _, arg_type = info in
            let new_offset = get_type_size ctx arg_type + curr_offset in
            ( new_offset,
              acc @ expr_instrs
              @ store_with_offset ctx arg_type "sp" new_offset 0 ))
      in
      let stack_move_amt = Int.round_up ~to_multiple_of:16 stack_move_amt in
      (* Then, return the location on the stack to start reading from *)
      (* TODO i think this doesn't work for empty structs, but I also don't think it matters? *)
      [ Instr.Mov (X7, Reg Sp) ]
      @ push_instrs
      @ [ Instr.Sub (Sp, Sp, Const stack_move_amt) ]
      @ [ Instr.Mov (X0, Reg X7) ]
  | Cast (_, _type, inner, _) ->
      (* TODO maybe some actual logic in here *)
      let%map inner_instrs = gen_expr ctx inner in
      inner_instrs

and gen_postfix ctx _type expr op =
  let%bind ex = gen_expr ctx expr in
  let get_var () =
    match expr with
    | Var (_, typ, id) -> (typ, id)
    | _ -> raise (Failure "unreachable")
  in
  match op with
  | Deref -> (
      match _type with
      | Struct t -> Ok ex
      | _ -> Ok (ex @ load_from_offset ctx _type "x0" 0 0))
  | Incr ->
      let var_type, var_id = get_var () in
      let%map store_instrs = store_var ctx var_type var_id in
      ex @ [ Instr.Raw "add x0, x0, #1" ] @ store_instrs
  | Decr ->
      let var_type, var_id = get_var () in
      let%map store_instrs = store_var ctx var_type var_id in
      ex @ [ Instr.Raw "sub x0, x0, #1" ] @ store_instrs

and gen_unary ctx op expr =
  let%bind ex = gen_expr ctx expr in
  match op with
  | Addr ->
      let%bind var_id =
        match expr with
        | Var (_, _, id) -> Ok id
        | _ -> Or_error.error_string "unreachable"
      in
      let%map { offset; _ } = lookup_var ctx var_id in
      [ Instr.Raw ("sub x0, fp, #" ^ Int.to_string offset) ]
      (* Put the addr of the var in x0 *)
  | Neg -> Ok (ex @ [ Instr.Neg (X0, Reg X0) ])
  | Bang ->
      Ok
        (ex
        @ [
            Instr.Cmp (X0, Const 0);
            Instr.Mov (X0, Const 0);
            Instr.CSet (X0, "eq");
          ])
  | Tilde -> Ok (ex @ [ Instr.Mvn (X0, Reg X0) ])

and gen_binary ctx e1 op e2 =
  let%bind e1_instrs = gen_expr ctx e1 in
  let%map e2_instrs = gen_expr ctx e2 in
  (* Most instrs we handle right now by pushing e1 on to the stack, putting e2 in x0, and popping e1 into x1
     and then operating on x1 and x0. This is just a shortcut to get everything before operating z*)
  let push_pop_instr_chain =
    e1_instrs @ [ Instr.Push X0 ] @ e2_instrs @ [ Instr.Pop X1 ]
  in

  match op with
  (* Arithmetic *)
  | Plus -> push_pop_instr_chain @ [ Instr.Add (X0, X0, Reg X1) ]
  | Minus -> push_pop_instr_chain @ [ Instr.Sub (X0, X1, Reg X0) ]
  | Star -> push_pop_instr_chain @ [ Instr.Mul (X0, X0, Reg X1) ]
  | Slash -> push_pop_instr_chain @ [ Instr.Div (X0, X1, Reg X0) ]
  | Mod ->
      push_pop_instr_chain
      @ [ Instr.Div (X2, X1, Reg X0); Instr.MSub (X0, X0, X2, X1) ]
  (* Bitwise *)
  | LShift -> push_pop_instr_chain @ [ Instr.Lsl (X0, X1, Reg X0) ]
  | RShift -> push_pop_instr_chain @ [ Instr.Lsr (X0, X1, Reg X0) ]
  | And -> push_pop_instr_chain @ [ Instr.And (X0, X1, Reg X0) ]
  | Or -> push_pop_instr_chain @ [ Instr.Orr (X0, X1, Reg X0) ]
  | Xor -> push_pop_instr_chain @ [ Instr.Eor (X0, X1, Reg X0) ]
  (* Logical *)
  | LAnd ->
      let check_label = gen_label "land_check" in
      let end_label = gen_label "land_end" in
      e1_instrs
      @ [
          Instr.Cmp (X0, Const 0);
          Instr.Bne check_label;
          Instr.B end_label;
          Instr.Label check_label;
        ]
      @ e2_instrs
      @ [
          Instr.Cmp (X0, Const 0);
          Instr.Mov (X0, Const 0);
          Instr.CSet (X0, "ne");
          Instr.Label end_label;
        ]
  | LOr ->
      let check_label = gen_label "land_check" in
      let end_label = gen_label "land_end" in
      e1_instrs
      @ [
          Instr.Cmp (X0, Const 0);
          Instr.Beq check_label;
          Instr.Mov (X0, Const 1);
          Instr.B end_label;
          Instr.Label check_label;
        ]
      @ e2_instrs
      @ [
          Instr.Cmp (X0, Const 0);
          Instr.Mov (X0, Const 0);
          Instr.CSet (X0, "ne");
          Instr.Label end_label;
        ]
  | Eq ->
      push_pop_instr_chain
      @ [
          Instr.Cmp (X1, Reg X0); Instr.Mov (X0, Const 0); Instr.CSet (X0, "eq");
        ]
  | Neq ->
      push_pop_instr_chain
      @ [
          Instr.Cmp (X1, Reg X0); Instr.Mov (X0, Const 0); Instr.CSet (X0, "ne");
        ]
  | Lt ->
      push_pop_instr_chain
      @ [
          Instr.Cmp (X1, Reg X0); Instr.Mov (X0, Const 0); Instr.CSet (X0, "lt");
        ]
  | Lte ->
      push_pop_instr_chain
      @ [
          Instr.Cmp (X1, Reg X0); Instr.Mov (X0, Const 0); Instr.CSet (X0, "le");
        ]
  | Gt ->
      push_pop_instr_chain
      @ [
          Instr.Cmp (X1, Reg X0); Instr.Mov (X0, Const 0); Instr.CSet (X0, "gt");
        ]
  | Gte ->
      push_pop_instr_chain
      @ [
          Instr.Cmp (X1, Reg X0); Instr.Mov (X0, Const 0); Instr.CSet (X0, "ge");
        ]

(*
  gen_stmt returns a list of instructions and an accumulated amount of stack used for local vars
*)
let rec gen_stmt (ctx : exec_context) (stack_used : int) (stmt : Stmt.t) :
    (exec_context * int * Instr.t list) Or_error.t =
  match stmt with
  | Block (_, stmts) ->
      List.fold stmts
        ~init:
          (Ok
             ( {
                 var_map = ctx.var_map;
                 curr_scope = Map.empty (module String);
                 struct_mapping = ctx.struct_mapping;
                 continue_label = ctx.continue_label;
                 break_label = ctx.break_label;
               },
               stack_used,
               [] ))
        ~f:(fun acc stmt ->
          let%bind ctx, old_vars, old_instrs = acc in
          let%map ctx, new_vars, instrs = gen_stmt ctx old_vars stmt in
          (ctx, new_vars, old_instrs @ instrs))
  | Expr (_, e) ->
      let%map e = gen_expr ctx e in
      (ctx, stack_used, e)
  | If (_, cond, if_true, if_false) ->
      gen_if ctx stack_used cond if_true if_false
  | For (_, decl, cond, post, body) ->
      gen_for ctx stack_used decl cond post body
  | While (_, cond, body) -> gen_while ctx stack_used cond body
  | Return (_, e) -> gen_return ctx stack_used e
  | Break _ -> (
      match ctx.break_label with
      | Some label -> Ok (ctx, stack_used, [ Instr.B label ])
      | None -> Or_error.error_string "Cannot use break in this context")
  | Continue _ -> (
      match ctx.continue_label with
      | Some label -> Ok (ctx, stack_used, [ Instr.B label ])
      | None -> Or_error.error_string "Cannot use break in this context")
  | Declaration a -> gen_declaration ctx stack_used a
  | Assignment (_, id, expr) -> gen_assignment ctx stack_used id expr

and gen_for ctx stack_used decl cond post body =
  let start_label = gen_label "for_start" in
  let end_label = gen_label "for_end" in
  let post_label = gen_label "for_post" in
  let with_cont_break =
    {
      curr_scope = ctx.curr_scope;
      var_map = ctx.var_map;
      struct_mapping = ctx.struct_mapping;
      break_label = Some end_label;
      continue_label = Some post_label;
    }
  in
  let%bind new_ctx, stack_used, decl_instrs =
    gen_stmt with_cont_break stack_used decl
  in
  let%bind _, stack_used, body_instrs = gen_stmt new_ctx stack_used body in
  let%bind cond = gen_expr new_ctx cond in
  let%map post = gen_expr new_ctx post in
  ( ctx,
    stack_used,
    decl_instrs
    @ [ Instr.Label start_label ]
    @ cond
    @ [ Instr.Cmp (X0, Const 0); Instr.Beq end_label ]
    @ body_instrs @ [ Instr.Label post_label ] @ post
    @ [ Instr.B start_label; Instr.Label end_label ] )

and gen_while ctx stack_used cond body =
  let start_label = gen_label "while_start" in
  let end_label = gen_label "while_end" in
  let with_cont_break =
    {
      curr_scope = ctx.curr_scope;
      var_map = ctx.var_map;
      struct_mapping = ctx.struct_mapping;
      break_label = Some end_label;
      continue_label = Some start_label;
    }
  in
  let%bind _, stack_used, body_instrs =
    gen_stmt with_cont_break stack_used body
  in
  let%map cond = gen_expr ctx cond in
  ( ctx,
    stack_used,
    [ Instr.Label start_label ]
    @ cond
    @ [ Instr.Cmp (X0, Const 0); Instr.Beq end_label ]
    @ body_instrs
    @ [ Instr.B start_label; Instr.Label end_label ] )

and gen_assignment ctx stack_used lhs expr =
  (* Current, lhs is either a pointer or a var; TODO we need special stuff for structs *)
  match lhs with
  | Var (_, _type, id) ->
      let%bind { offset; _ } = lookup_var ctx id in
      let%map expr = gen_expr ctx expr in
      (ctx, stack_used, expr @ store_with_offset ctx _type "fp" offset 0)
  | PostFix (_, _, inner, Deref) ->
      let%bind inner_instrs = gen_expr ctx inner in
      let%map rhs_instrs = gen_expr ctx expr in
      ( ctx,
        stack_used,
        inner_instrs @ [ Instr.Push X0 ] @ rhs_instrs @ [ Instr.Pop X1 ]
        (* At this point, the location of the ptr is in x1 and the expr value is in x0 *)
        @ [ Instr.Raw "str w0, [x1]" ] )
  | _ -> Or_error.error_string "unreachable"

and gen_declaration ctx stack_used
    ({ span = _; mut = _; id; type_annotation; defn } : Stmt.declaration) =
  let decl_type =
    match type_annotation with
    | Some t -> t
    | None -> (
        match defn with
        | Some defn -> Expr.get_type defn
        | None -> raise (Failure "unreachable"))
  in
  let stack_required = get_type_size ctx decl_type in
  let offset = stack_used + stack_required in
  let var_map = Map.set ctx.var_map ~key:id ~data:{ offset; typ = decl_type } in
  let curr_scope =
    Map.set ctx.curr_scope ~key:id ~data:{ offset; typ = decl_type }
  in
  let new_ctx =
    {
      var_map;
      curr_scope;
      struct_mapping = ctx.struct_mapping;
      continue_label = ctx.continue_label;
      break_label = ctx.break_label;
    }
  in
  let%map instrs =
    match defn with
    | Some e -> (
        let%bind assignment_expr = gen_expr ctx e in
        (* use old ctx here *)
        let _type = Expr.get_type e in
        match _type with
        | Struct id ->
            let struct_info = Map.find_exn ctx.struct_mapping id in
            let _, struct_push_instrs =
              List.fold struct_info ~init:(0, [])
                ~f:(fun (cum_size, acc) (_, _type) ->
                  let size = cum_size + get_type_size new_ctx _type in
                  (* Note that x0 stores the area in the stack where we should begin looking *)
                  ( size,
                    acc
                    @ load_from_offset new_ctx _type "x0" size 1
                    @ (* At this point, x1 holds the current member to store. *)
                    store_with_offset new_ctx _type "fp" (offset + size) 1 ))
            in
            Ok (assignment_expr @ struct_push_instrs)
        | _ ->
            let%map store_instrs = store_var new_ctx _type id in
            assignment_expr @ store_instrs
        (* At this point, we have stored the value where it needs to go, and we can move on*)
        )
    | None -> Ok []
  in
  (new_ctx, offset, instrs)

and gen_return ctx stack_used e =
  let epilogue : Instr.t list =
    [ Mov (Sp, Reg Fp); Raw "ldp x29, x30, [sp], 16"; Ret ]
  in
  let%map expr_instrs =
    match e with Some e -> gen_expr ctx e | None -> Ok []
  in
  (ctx, stack_used, expr_instrs @ epilogue)

and gen_if ctx stack_used cond if_true if_false =
  let false_label = gen_label "if_false" in
  let end_label = gen_label "if_end" in
  let jmp_label =
    match if_false with Some _ -> false_label | None -> end_label
  in
  let%bind cond_instrs = gen_expr ctx cond in
  let%bind _, stack_used, if_true_instrs = gen_stmt ctx stack_used if_true in
  let%bind _, stack_used, if_false_instrs =
    match if_false with
    | Some if_false ->
        let%map _, stack_space, instrs = gen_stmt ctx stack_used if_false in
        (ctx, stack_space, [ Instr.Label false_label ] @ instrs)
    | None -> Ok (ctx, stack_used, [])
  in
  let instrs =
    cond_instrs
    @ [ Instr.Cmp (X0, Const 0); Instr.Beq jmp_label ]
    @ if_true_instrs @ [ Instr.B end_label ] @ if_false_instrs
    @ [ Instr.Label end_label ]
  in
  Ok (ctx, stack_used, instrs)

let gen_fn (ctx : exec_context) (fn : Fn.t) : Instr.t list Or_error.t =
  (* This is a bit of a hack, but all args will have a declaration associated
      which will update var_map as needed, and we will also generate
     a series of movs which move into the correct places
  *)
  let arg_stmts =
    Stmt.Block
      ( fn.span,
        List.map fn.args ~f:(fun (span, id, _type) ->
            Stmt.Declaration
              {
                span;
                mut = Const;
                id;
                type_annotation = Some _type;
                defn = None;
              }) )
  in
  (* 16 bytes is always reserved at the start for fp *)
  let%bind ctx, stack_required, arg_instrs = gen_stmt ctx 16 arg_stmts in
  assert (List.is_empty arg_instrs);
  let%bind arg_ldr =
    List.foldi fn.args ~init:(Ok []) ~f:(fun idx acc (_, id, _type) ->
        let%bind acc = acc in
        let%map { offset; _ } = lookup_var ctx id in
        acc @ store_with_offset ctx _type "fp" offset idx)
  in
  let%map _ctx, stack_required, body = gen_stmt ctx stack_required fn.body in
  let preamble : Instr.t list =
    [
      Instr.Label fn.id;
      Instr.Raw "stp fp, x30, [sp, #-16]!";
      Instr.Mov (Fp, Reg Sp);
    ]
    @ arg_ldr
    @
    if stack_required > 0 then
      let stack_required = Int.round_up ~to_multiple_of:16 stack_required in
      [ Instr.Sub (Sp, Sp, Const stack_required) ]
    else []
  in
  (* _print_map _ctx.var_map; *)
  preamble @ body

let gen_translation_unit (trans : translation_unit) : Instr.t list Or_error.t =
  let struct_mapping = get_struct_map trans in
  let empty_ctx =
    {
      var_map = Map.empty (module String);
      curr_scope = Map.empty (module String);
      struct_mapping;
      continue_label = None;
      break_label = None;
    }
  in
  let%map fn_instrs =
    List.fold trans ~init:(Ok []) ~f:(fun acc tl ->
        let%bind acc = acc in
        match tl with
        | Fn fn ->
            let%map fn_res = gen_fn empty_ctx fn in
            acc @ fn_res
        | Struct _ -> Ok [])
  in
  let premable =
    [ Instr.Raw ".text" ]
    @ List.map !string_list ~f:(fun (label, str) ->
          Instr.Raw (".align 4\n." ^ label ^ ": .string \"" ^ str ^ "\""))
    @ [
        Instr.Raw ".globl _start";
        Instr.Raw ".align 4";
        Instr.Raw "_start:";
        Instr.Raw "bl main";
        Instr.Mov (X16, Const 1);
        Instr.Svc (Const 0);
      ]
  in
  premable @ fn_instrs
