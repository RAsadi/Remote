%{
  open Ast
%}

%token <Ast.literal> Literal
%token <Ast.identifier> Iden
%token Eof

// Keywords
%token Return
%token If
%token Else
%token For
%token While
%token Break
%token Continue
%token Fn
%token U32
%token Let
%token Mut
%token In
%token Sizeof

// Punc
%token LBrace
%token RBrace
%token LParen
%token RParen
%token Semi
%token Colon
%token Arrow
%token Comma

////////////// Operators

// Arithmetic
%token Minus
%token Bang
%token Tilde
%token Star
%token Slash
%token Plus
%token Incr
%token Decr

// Bitwise
%token LShift
%token RShift
%token And
%token Or
%token Xor

// Logical
%token LAnd
%token LOr

// Relational
%token Eq
%token Neq
%token Gt
%token Gte
%token Lt
%token Lte

// Assignment
%token Assign

%start <Ast.translation_unit> translation_unit
%%

let translation_unit := ~ = list(top_level_element); Eof; <>

let top_level_element :=
  | ~ = fn; <Fn>

let fn := 
  | Fn; id = Iden; LParen; args = separated_list(Comma, type_binding); RParen; sg = option(type_signature); body = compound_stmt;
    { {id=id; args=args; _type=sg; body=body } }

let type_name ==
  | U32; { U32 }

let type_annoation == Colon; ~ = type_name; <>

let type_binding := id = Iden; typ = type_annoation; <>

let type_signature == Arrow; ~ = type_name; <>


// Statements

// TODO match

let stmt :=
  | expr_stmt
  | compound_stmt
  | iteration_stmt
  | assignment_stmt
  | selection_stmt
  | jump_stmt

let jump_stmt :=
  | Return; ~ = option(expr); Semi; <Return>
  | Break; Semi; { Break }
  | Continue; Semi; { Continue }

let compound_stmt := LBrace; ~ = list(stmt); RBrace; <Block>

let expr_stmt := ~ = expr; Semi; <Expr>

let assignment_stmt :=
  | Let; mut = option(Mut); id = Iden; annotation = option(type_annoation); Assign; e = expr; Semi;
    { Assignment {is_mut=(match mut with Some _ -> true | None -> false); id=id; type_annotation=annotation; defn=Some e}}
  | Let; mut = option(Mut); id = Iden; annotation = option(type_annoation); Semi;
    { Assignment {is_mut=(match mut with Some _ -> true | None -> false); id=id; type_annotation=annotation; defn=None}}

let selection_stmt :=
  | If; e = expr; body = compound_stmt; { If (e, body, None) }
  | If; e = expr; body = compound_stmt; Else; els = else_stmt; { If (e, body, Some els) }

let else_stmt :=
  | compound_stmt
  | selection_stmt

let iteration_stmt :=
  | for_stmt
  | while_stmt

let for_stmt := For; id = Iden; In; cond = expr; body = compound_stmt; <For>

let while_stmt := While; cond = expr; body = compound_stmt; <While>


////////// Expressions

let primary_expr :=
  | ~ = Iden; <Var>
  | ~ = Literal; <Literal>
  | LParen; ~ = expr; RParen; <>
  | id = Iden; LParen; args = separated_list(Comma, expr); RParen; <Call>

let postfix_expr :=
  | primary_expr
  | ~ = postfix_expr; ~ = postfix_op; <PostFix>

let unary_expr :=
  | postfix_expr
  | ~ = unary_op; ~ = unary_expr; <Unary>
  | Sizeof; LParen; id = Iden; RParen; <Sizeof>

let multiplicative_expr :=
  | unary_expr
  | e1 = multiplicative_expr; op = multiplicative_op; e2 = unary_expr; <Binary>

let additive_expr :=
  | multiplicative_expr
  | e1 = additive_expr; op = additive_op; e2 = multiplicative_expr; <Binary>

let shift_expr :=
  | additive_expr
  | e1 = shift_expr; op = shift_op; e2 = additive_expr; <Binary>

let relation_expr :=
  | shift_expr
  | e1 = relation_expr; op = relational_op; e2 = shift_expr; <Binary>

let equality_expr :=
  | relation_expr
  | e1 = equality_expr; op = equality_op; e2 = relation_expr; <Binary>

let and_expr :=
  | equality_expr
  | e1 = and_expr; And; e2 = equality_expr; { Binary(e1, And, e2) }

let xor_expr :=
  | and_expr
  | e1 = xor_expr; Xor; e2 = and_expr; { Binary(e1, Xor, e2) }

let or_expr :=
  | xor_expr
  | e1 = or_expr; Or; e2 = xor_expr; { Binary(e1, Or, e2) }

let land_expr :=
  | or_expr
  | e1 = land_expr; LAnd; e2 = or_expr; { Binary(e1, LAnd, e2) }

let lor_expr :=
  | land_expr
  | e1 = lor_expr; LOr; e2 = land_expr; { Binary(e1, LOr, e2) }

let expr == lor_expr

let unary_op ==
  | Minus; { Neg }
  | Bang; { Bang }
  | Tilde; { Tilde }

let multiplicative_op ==
  | Star; { Star }
  | Slash; { Slash }

let additive_op ==
  | Plus; { Plus }
  | Minus; { Minus }

let relational_op ==
  | Gt; { Gt }
  | Gte; { Gte }
  | Lt; { Lt }
  | Lte; { Lte }

let equality_op ==
  | Eq; { Eq }
  | Neq; { Neq }

let shift_op ==
  | LShift; { LShift }
  | RShift; { RShift }

let postfix_op ==
  | Incr; { Incr }
  | Decr; { Decr }
