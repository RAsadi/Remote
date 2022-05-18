open Typing.Typed_ast
open Ast.Ast_types
open Base
open Base.Result.Let_syntax

type variable_mapping = (string, int, String.comparator_witness) Map.t
type struct_type_info = (string * _type) list

type struct_mapping =
  (string, struct_type_info, String.comparator_witness) Map.t

type exec_context = {
  var_map : variable_mapping;
  curr_scope : variable_mapping;
  struct_mapping : struct_mapping;
  continue_label : string option;
  break_label : string option;
}

let rec find_idx lst x =
  match lst with
  | [] -> raise (Failure "HHMG")
  | h :: t -> if String.equal x h then 0 else 1 + find_idx t x

let get_offset_from_field ctx struct_type field_name =
  match struct_type with
  | Struct struct_id ->
      let struct_info = Map.find_exn ctx.struct_mapping struct_id in
      let offset =
        find_idx (List.map struct_info ~f:(fun (id, _) -> id)) field_name
      in
      offset * 16
  | _ -> raise (Failure "unrentarst")

let rec get_type_size ctx _type =
  match _type with
  | U32 -> 1
  | Bool -> 1
  | Void -> 0
  | Pointer _ -> 1
  | Struct id ->
      let members = Map.find_exn ctx.struct_mapping id in
      List.fold members ~init:0 ~f:(fun acc (_, member) ->
          acc + get_type_size ctx member)

let push reg_list = List.map reg_list ~f:(fun reg -> Instr.Push reg)
let pop reg_list = List.map reg_list ~f:(fun reg -> Instr.Pop reg)
let label_count = ref 0

(* Returns a new, unique label *)
let gen_label label_name =
  Int.incr label_count;
  label_name ^ Int.to_string !label_count

let lookup_var (ctx : exec_context) id : int Or_error.t =
  match Map.find ctx.curr_scope id with
  | Some offset -> Ok offset
  | None -> Ok (Map.find_exn ctx.var_map id)
(* We know it has to exist because of type checking *)

let rec gen_expr (ctx : exec_context) (expr : expr) : Instr.t list Or_error.t =
  match expr with
  | Literal (_, _, l) -> (
      match l with
      | U32 i -> Ok [ Instr.Mov (Real X0, Const i) ]
      | Bool b -> Ok [ Instr.Mov (Real X0, Const (Bool.to_int b)) ])
  | Unary (_, _, op, e) -> gen_unary ctx op e
  | Binary (_, _, e1, op, e2) -> gen_binary ctx e1 op e2
  | Var (_, _type, id) -> (
      match _type with
      | Struct _ ->
          let%map offset = lookup_var ctx id in
          [ Instr.Raw ("sub x0, fp, #" ^ Int.to_string offset) ]
      | _ ->
          let%map offset = lookup_var ctx id in
          [ Instr.Raw ("ldr x0, [fp, #-" ^ Int.to_string offset ^ "]") ])
  | Sizeof _ -> Ok []
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
              acc @ expr_instrs @ [ Instr.Push (Real X0) ])
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
        | _ -> Or_error.error_string "TODO cannot handle non var lhs"
      in
      let%map lhs_instrs = gen_expr ctx lhs in
      (* Based on codegen for var, x0 should now hold the address of var *)
      let field_offset = get_offset_from_field ctx struct_type field_name in
      lhs_instrs
      @ [ Instr.Raw ("ldr x0, [x0, #-" ^ Int.to_string field_offset ^ "]") ]
  | Initializer (_, _type, _, inits) ->
      (* Generate and push things onto the stack.
         This is technically broken in the case of returning from a function, but thats ok for now.
         Note that stack space is only reclaimed after the function exits, which is kinda scuffed
      *)
      let%map push_instrs =
        List.fold inits ~init:(Ok []) ~f:(fun acc arg ->
            let%bind acc = acc in
            let%map expr_instrs = gen_expr ctx arg in
            acc @ expr_instrs @ [ Instr.Push (Real X0) ])
      in
      (* Then, return the location on the stack to start reading from *)
      (* TODO i think this doesn't work for empty structs, but I also don't think it matters? *)
      [ Instr.Sub (Real X7, Real Sp, Const 16) ]
      @ push_instrs
      @ [ Instr.Mov (Real X0, Reg (Real X7)) ]

and gen_postfix ctx _type expr op =
  let%map ex = gen_expr ctx expr in
  ex
  @
  match op with
  | Deref -> [ Instr.Raw "ldr x0, [x0]" ]
  | Incr -> [ Instr.Raw "add x0, x0, #1" ]
  | Decr -> [ Instr.Raw "sub x0, x0, #1" ]

and gen_unary ctx op expr =
  let%bind ex = gen_expr ctx expr in
  match op with
  | Addr ->
      let%bind var_id =
        match expr with
        | Var (_, _, id) -> Ok id
        | _ -> Or_error.error_string "unreachable"
      in
      let%map offset = lookup_var ctx var_id in
      [ Instr.Raw ("sub x0, fp, #" ^ Int.to_string offset) ]
      (* Put the addr of the var in x0 *)
  | Neg -> Ok (ex @ [ Instr.Neg (Real X0, Reg (Real X0)) ])
  | Bang ->
      Ok
        (ex
        @ [
            Instr.Cmp (Real X0, Const 0);
            Instr.Mov (Real X0, Const 0);
            Instr.CSet (Real X0, "eq");
          ])
  | Tilde -> Ok (ex @ [ Instr.Mvn (Real X0, Reg (Real X0)) ])

and gen_binary ctx e1 op e2 =
  let%bind e1_instrs = gen_expr ctx e1 in
  let%map e2_instrs = gen_expr ctx e2 in
  (* Most instrs we handle right now by pushing e1 on to the stack, putting e2 in x0, and popping e1 into x1
     and then operating on x1 and x0. This is just a shortcut to get everything before operating z*)
  let push_pop_instr_chain =
    e1_instrs @ [ Instr.Push (Real X0) ] @ e2_instrs @ [ Instr.Pop (Real X1) ]
  in

  match op with
  (* Arithmetic *)
  | Plus ->
      push_pop_instr_chain @ [ Instr.Add (Real X0, Real X0, Reg (Real X1)) ]
  | Minus ->
      push_pop_instr_chain @ [ Instr.Sub (Real X0, Real X1, Reg (Real X0)) ]
  | Star ->
      push_pop_instr_chain @ [ Instr.Mul (Real X0, Real X0, Reg (Real X1)) ]
  | Slash ->
      push_pop_instr_chain @ [ Instr.Div (Real X0, Real X1, Reg (Real X0)) ]
  (* Bitwise *)
  | LShift ->
      push_pop_instr_chain @ [ Instr.Lsl (Real X0, Real X1, Reg (Real X0)) ]
  | RShift ->
      push_pop_instr_chain @ [ Instr.Lsr (Real X0, Real X1, Reg (Real X0)) ]
  | And ->
      push_pop_instr_chain @ [ Instr.And (Real X0, Real X1, Reg (Real X0)) ]
  | Or -> push_pop_instr_chain @ [ Instr.Orr (Real X0, Real X1, Reg (Real X0)) ]
  | Xor ->
      push_pop_instr_chain @ [ Instr.Eor (Real X0, Real X1, Reg (Real X0)) ]
  (* Logical *)
  | LAnd ->
      let check_label = gen_label "land_check" in
      let end_label = gen_label "land_end" in
      e1_instrs
      @ [
          Instr.Cmp (Real X0, Const 0);
          Instr.Bne check_label;
          Instr.B end_label;
          Instr.Label check_label;
        ]
      @ e2_instrs
      @ [
          Instr.Cmp (Real X0, Const 0);
          Instr.Mov (Real X0, Const 0);
          Instr.CSet (Real X0, "ne");
          Instr.Label end_label;
        ]
  | LOr ->
      let check_label = gen_label "land_check" in
      let end_label = gen_label "land_end" in
      e1_instrs
      @ [
          Instr.Cmp (Real X0, Const 0);
          Instr.Beq check_label;
          Instr.Mov (Real X0, Const 1);
          Instr.B end_label;
          Instr.Label check_label;
        ]
      @ e2_instrs
      @ [
          Instr.Cmp (Real X0, Const 0);
          Instr.Mov (Real X0, Const 0);
          Instr.CSet (Real X0, "ne");
          Instr.Label end_label;
        ]
  | Eq ->
      push_pop_instr_chain
      @ [
          Instr.Cmp (Real X1, Reg (Real X0));
          Instr.Mov (Real X0, Const 0);
          Instr.CSet (Real X0, "eq");
        ]
  | Neq ->
      push_pop_instr_chain
      @ [
          Instr.Cmp (Real X1, Reg (Real X0));
          Instr.Mov (Real X0, Const 0);
          Instr.CSet (Real X0, "ne");
        ]
  | Lt ->
      push_pop_instr_chain
      @ [
          Instr.Cmp (Real X1, Reg (Real X0));
          Instr.Mov (Real X0, Const 0);
          Instr.CSet (Real X0, "lt");
        ]
  | Lte ->
      push_pop_instr_chain
      @ [
          Instr.Cmp (Real X1, Reg (Real X0));
          Instr.Mov (Real X0, Const 0);
          Instr.CSet (Real X0, "le");
        ]
  | Gt ->
      push_pop_instr_chain
      @ [
          Instr.Cmp (Real X1, Reg (Real X0));
          Instr.Mov (Real X0, Const 0);
          Instr.CSet (Real X0, "gt");
        ]
  | Gte ->
      push_pop_instr_chain
      @ [
          Instr.Cmp (Real X1, Reg (Real X0));
          Instr.Mov (Real X0, Const 0);
          Instr.CSet (Real X0, "ge");
        ]

(*
  gen_stmt returns a list of instructions and the number of new variables defined
  The number of vars is used for allocating space for local vars
*)
let with_cont_break ctx continue_label break_label =
  {
    curr_scope = ctx.curr_scope;
    var_map = ctx.var_map;
    struct_mapping = ctx.struct_mapping;
    break_label = Some break_label;
    continue_label = Some continue_label;
  }

let rec gen_stmt (ctx : exec_context) (curr_local_vars : int) (stmt : stmt) :
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
               curr_local_vars,
               [] ))
        ~f:(fun acc stmt ->
          let%bind ctx, old_vars, old_instrs = acc in
          let%map ctx, new_vars, instrs = gen_stmt ctx old_vars stmt in
          (ctx, new_vars, old_instrs @ instrs))
  | Expr (_, e) ->
      let%map e = gen_expr ctx e in
      (ctx, curr_local_vars, e)
  | If (_, cond, if_true, if_false) ->
      gen_if ctx curr_local_vars cond if_true if_false
  | For _ -> Ok (ctx, curr_local_vars, []) (* TODO *)
  | While (_, cond, body) -> gen_while ctx curr_local_vars cond body
  | Return (_, e) -> gen_return ctx curr_local_vars e
  | Break _ -> (
      match ctx.break_label with
      | Some label -> Ok (ctx, curr_local_vars, [ Instr.B label ])
      | None -> Or_error.error_string "Cannot use break in this context")
  | Continue _ -> (
      match ctx.continue_label with
      | Some label -> Ok (ctx, curr_local_vars, [ Instr.B label ])
      | None -> Or_error.error_string "Cannot use break in this context")
  | Declaration a -> gen_declaration ctx curr_local_vars a
  | Assignment (_, id, expr) -> gen_assignment ctx curr_local_vars id expr

and gen_while ctx curr_local_vars cond body =
  let start_label = gen_label "while_start" in
  let end_label = gen_label "while_end" in
  let%bind _, curr_local_vars, body_instrs =
    gen_stmt (with_cont_break ctx start_label end_label) curr_local_vars body
  in
  let%map cond = gen_expr ctx cond in
  ( ctx,
    curr_local_vars,
    [ Instr.Label start_label ]
    @ cond
    @ [ Instr.Cmp (Real X0, Const 0); Instr.Beq end_label ]
    @ body_instrs
    @ [ Instr.B start_label; Instr.Label end_label ] )

and gen_assignment ctx curr_local_vars lhs expr =
  (* Current, lhs is either a pointer or a var; TODO we need special stuff for structs *)
  match lhs with
  | Var (_, _, id) ->
      let%bind offset = lookup_var ctx id in
      let%map expr = gen_expr ctx expr in
      ( ctx,
        curr_local_vars,
        expr @ [ Instr.Raw ("str x0, [fp, #-" ^ Int.to_string offset ^ "]") ] )
  | PostFix (_, _, inner, Deref) ->
      let%bind inner_instrs = gen_expr ctx inner in
      let%map rhs_instrs = gen_expr ctx expr in
      ( ctx,
        curr_local_vars,
        inner_instrs @ [ Instr.Push (Real X0) ] @ rhs_instrs
        @ [ Instr.Pop (Real X1) ]
        (* At this point, the location of the ptr is in x1 and the expr value is in x0 *)
        @ [ Instr.Raw "str x0, [x1]" ] )
  | _ -> Or_error.error_string "unreachable"

and gen_declaration ctx curr_local_vars
    ({ span = _; mut = _; id; type_annotation; defn } : declaration) =
  let decl_type _type_annotation defn : _type =
    match _type_annotation with
    | Some t -> t
    | None -> (
        match defn with
        | Some defn -> get_expr_type defn
        | None -> raise (Failure "unreachable"))
  in
  let offset = (curr_local_vars + 1) * 16 in
  let var_map = Map.set ctx.var_map ~key:id ~data:offset in
  let curr_scope = Map.set ctx.curr_scope ~key:id ~data:offset in
  let%map instrs =
    match defn with
    | Some e ->
        let%map assignment_expr = gen_expr ctx e in
        let _type = get_expr_type e in
        let type_size = get_type_size ctx _type in
        if type_size = 1 then
          assignment_expr
          @ [ Instr.Raw ("str x0, [fp, #-" ^ Int.to_string offset ^ "]") ]
        else
          assignment_expr
          @ List.fold (List.range 0 type_size) ~init:[] ~f:(fun acc idx ->
                (* Note that x0 stores the area in the stack where we should begin looking *)
                acc
                @ [
                    Instr.Raw
                      ("ldr x1, [x0, #-" ^ Int.to_string (idx * 16) ^ "]");
                  ]
                @ (* At this point, x1 holds the current member to store. *)
                [
                  Instr.Raw
                    ("str x1, [fp, #-"
                    ^ Int.to_string (offset + (idx * 16))
                    ^ "]");
                ])
        (* At this point, we have stored the value where it needs to go, and we can move on*)
    | None -> Ok []
  in
  ( {
      var_map;
      curr_scope;
      struct_mapping = ctx.struct_mapping;
      continue_label = ctx.continue_label;
      break_label = ctx.break_label;
    },
    curr_local_vars + get_type_size ctx (decl_type type_annotation defn),
    instrs )

and gen_return ctx curr_local_vars e =
  let epilogue : Instr.t list =
    [ Mov (Real Sp, Reg (Real Fp)); Raw "ldp x29, x30, [sp], 16"; Ret ]
  in
  let%map expr_instrs =
    match e with Some e -> gen_expr ctx e | None -> Ok []
  in
  (ctx, curr_local_vars, expr_instrs @ epilogue)

and gen_if ctx curr_local_vars cond if_true if_false =
  let false_label = gen_label "if_false" in
  let end_label = gen_label "if_end" in
  let jmp_label =
    match if_false with Some _ -> false_label | None -> end_label
  in
  let%bind cond_instrs = gen_expr ctx cond in
  let%bind _, if_true_vars, if_true_instrs =
    gen_stmt ctx curr_local_vars if_true
  in
  let%bind _, if_false_vars, if_false_instrs =
    match if_false with
    | Some if_false ->
        let%map _, num_vars, instrs = gen_stmt ctx curr_local_vars if_false in
        (ctx, num_vars, [ Instr.Label false_label ] @ instrs)
    | None -> Ok (ctx, curr_local_vars, [])
  in
  let total_num_vars = if_true_vars + if_false_vars in
  let instrs =
    cond_instrs
    @ [ Instr.Cmp (Real X0, Const 0); Instr.Beq jmp_label ]
    @ if_true_instrs @ [ Instr.B end_label ] @ if_false_instrs
    @ [ Instr.Label end_label ]
  in
  Ok (ctx, total_num_vars, instrs)

let gen_fn (ctx : exec_context) (fn : fn) : Instr.t list Or_error.t =
  (* This is a bit of a hack, but all args will have a declaration associated
      which will update var_map as needed, and we will also generate
     a series of movs which move into the correct places
  *)
  let arg_stmts =
    Block
      ( fn.span,
        List.map fn.args ~f:(fun (span, id, _type) ->
            Declaration
              {
                span;
                mut = Const;
                id;
                type_annotation = Some _type;
                defn = None;
              }) )
  in
  let%bind ctx, num_local_vars, arg_instrs = gen_stmt ctx 0 arg_stmts in
  assert (List.is_empty arg_instrs);
  let%bind arg_ldr =
    List.mapi fn.args ~f:(fun idx (_, id, _) ->
        let%map offset = lookup_var ctx id in
        Instr.Raw
          ("str x" ^ Int.to_string idx ^ ", [fp, #-" ^ Int.to_string offset
         ^ "]"))
    |> Result.all
  in
  let%map _, num_local_vars, body = gen_stmt ctx num_local_vars fn.body in
  let preamble : Instr.t list =
    [
      Instr.Label fn.id;
      Instr.Raw "stp fp, x30, [sp, #-16]!";
      Instr.Mov (Real Fp, Reg (Real Sp));
    ]
    @ arg_ldr
    @
    if num_local_vars > 0 then
      [ Instr.Sub (Real Sp, Real Sp, Const (num_local_vars * 16)) ]
    else []
  in
  preamble @ body

let gen_translation_unit (trans : translation_unit) : Instr.t list Or_error.t =
  let premable : Instr.t list =
    [
      Raw ".data";
      Raw ".text";
      Raw ".globl _start";
      Raw ".align 4";
      Raw "_start:";
      Raw "bl main";
      Mov (Real X16, Const 1);
      Svc (Const 0);
    ]
  in
  let struct_mapping = Typing.Type_check.get_struct_map trans in
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
  premable @ fn_instrs
