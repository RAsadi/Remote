type mutability = Mut | Const [@@deriving sexp, compare, equal]

type t = U32 | U8 | Bool | Void | Struct of Identifier.t | Pointer of t
[@@deriving sexp, compare, equal]
