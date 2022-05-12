%{
  
%}

%token Literal
%token Iden
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
%token RBrace
%token LBrace
%token RParen
%token LParen
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

%start <unit> translation_unit
%%

let translation_unit := list(top_level_element); Eof; {}

let top_level_element :=
  | fn

let fn := Fn; Iden; LParen; separated_list(Comma, type_binding); RParen; option(type_signature); compound_stmt; {}

let type_name ==
  | U32

let type_annoation == Colon; type_name; {}

let type_binding := Iden; type_annoation; {}

let type_signature == Arrow; type_name; {}


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
  | Return; option(expr); Semi; {}
  | Break; Semi; {}
  | Continue; Semi; {}

let compound_stmt := LBrace; list(stmt); RBrace; {}

let expr_stmt := expr; Semi; {}

let assignment_stmt := 
  | Let; option(Mut); Iden; option(type_annoation); Assign; expr; Semi; {}

let selection_stmt :=
  | If; LParen; expr; RParen; compound_stmt; {}
  | If; LParen; expr; RParen; compound_stmt; Else; else_stmt; {}

let else_stmt := 
  | compound_stmt
  | selection_stmt

let iteration_stmt :=
  | for_stmt
  | while_stmt

let for_stmt := For; Iden; In; expr; compound_stmt; {}

let while_stmt := While; expr; compound_stmt; {}



////////// Expressions

let primary_expr :=
  | Iden; {}
  | Literal; {}
  | LParen; expr; RParen; {}
  | Iden; LParen; separated_list(Comma, expr); RParen; {}

let postfix_expr :=
  | primary_expr
  | postfix_expr; postfix_op; {}

let unary_expr :=
  | postfix_expr
  | unary_op; unary_expr; {}
  | Sizeof; LParen; Iden; RParen; {}

let multiplicative_expr :=
  | unary_expr
  | e1 = multiplicative_expr; op = multiplicative_op; e2 = unary_expr; {}

let additive_expr :=
  | multiplicative_expr
  | e1 = additive_expr; op = additive_op; e2 = multiplicative_expr; {}

let shift_expr :=
  | additive_expr
  | e1 = shift_expr; op = shift_op; e2 = additive_expr; {}

let relation_expr :=
  | shift_expr
  | e1 = relation_expr; op = relational_op; e2 = shift_expr; {}

let equality_expr :=
  | relation_expr
  | e1 = equality_expr; op = equality_op; e2 = relation_expr; {}

let and_expr :=
  | equality_expr
  | and_expr; And; equality_expr; {}

let xor_expr :=
  | and_expr
  | xor_expr; Xor; and_expr; {}

let or_expr :=
  | xor_expr
  | or_expr; Or; xor_expr; {}

let land_expr :=
  | or_expr
  | e1 = land_expr; LAnd; e2 = or_expr; {}

let lor_expr :=
  | land_expr
  | e1 = lor_expr; LOr; e2 = land_expr; {}

let expr == lor_expr

let unary_op ==
  | Minus; { }
  | Bang; { }
  | Tilde; { }

let multiplicative_op ==
  | Star; { }
  | Slash; { }

let additive_op ==
  | Plus; { }
  | Minus; { }

let relational_op ==
  | Gt; { }
  | Gte; { }
  | Lt; { }
  | Lte; { }

let equality_op ==
  | Eq; { }
  | Neq; { }

let shift_op ==
  | LShift; { }
  | RShift; { }

let postfix_op ==
  | Incr; { }
  | Decr; { }