open Base

type mutability = Mut | Const [@@deriving sexp, compare, equal]

type t = U32 | U8 | Bool | Void | Struct of Identifier.t | Pointer of t
[@@deriving sexp, compare, equal]

let max_u8 = Int.pow 2 8 - 1
let max_u32 = Int.pow 2 32 - 1

let smallest_numeric i =
  if i <= max_u8 then U8
  else if i <= max_u32 then U32
  else raise (Failure "he numeric too big")

let is_numeric _type = match _type with U8 | U32 -> true | _ -> false

let bigger_numeric t1 t2 =
  match (t1, t2) with
  | U32, U32 | U32, U8 | U8, U32 -> U32
  | U8, U8 -> U8
  | _ -> raise (Failure "unreachable")

(* Checks if t2 is convertable to t1. Note this doesn't commute  *)
let can_convert t1 t2 =
  let can_widen t1 t2 =
    is_numeric t1 && is_numeric t2 && equal t1 (bigger_numeric t1 t2)
  in
  equal t1 t2 || can_widen t1 t2

(* Checks if type_list2 is convertable to type_list1 *)
let can_convert_list type_list1 type_list2 =
  match List.zip type_list1 type_list2 with
  | Ok lst ->
      List.fold lst ~init:true ~f:(fun acc (t1, t2) -> acc && can_convert t1 t2)
  | List.Or_unequal_lengths.Unequal_lengths -> false

