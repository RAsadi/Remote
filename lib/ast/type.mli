type mutability = Mut | Const [@@deriving sexp, compare, equal]

type t = U32 | U8 | Bool | Void | Struct of string | Pointer of t
[@@deriving sexp, compare, equal]

val max_u8 : int
val max_u32 : int
val smallest_numeric : int -> t
val is_numeric : t -> bool
val bigger_numeric : t -> t -> t
val can_convert : t -> t -> bool
val can_convert_list : t list -> t list -> bool
