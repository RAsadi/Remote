%{
  open Parsed_ast
%}

%token <Ast.Literal.t> Literal
%token <Ast.Identifier.t> Iden
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
%token Let
%token Mut
%token Sizeof
%token Struct
%token As

// Types
%token U32
%token Bool
%token U8

// Punc
%token LBrace
%token RBrace
%token LParen
%token RParen
%token LSquare
%token RSquare
%token Semi
%token Colon
%token Arrow
%token Comma
%token Dot

////////////// Operators

// Pointer
%token Caret
%token Ampersand

// Arithmetic
%token Minus
%token Bang
%token Tilde
%token Star
%token Slash
%token Plus
%token Incr
%token Decr
%token Mod

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

%type <Ast.Type.t> type_name

%start <Parsed_ast.translation_unit> translation_unit
%%

let translation_unit := ~ = list(top_level_element); Eof; <>

let top_level_element :=
  | ~ = fn; <TopLevelElement.Fn>
  | ~ = _struct; <TopLevelElement.Struct>

let fn := Fn; id = Iden; LParen; args = separated_list(Comma, type_binding); RParen; sg = option(type_signature); body = compound_stmt;
    { {span=($startpos, $endpos); id=id; args=args; typ=(match sg with Some sg -> sg | None -> Void); body=body } }

let _struct := Struct; id = Iden; LBrace; types = separated_list(Comma, type_binding); RBrace;
  { (($startpos, $endpos), id, types) }

let type_name :=
  | U32; { U32 }
  | Bool; { Bool }
  | U8; { U8 }
  | ~ = Iden; <Ast.Type.Struct>
  | inner=type_name; Caret; <Ast.Type.Pointer>

let type_annotation == Colon; ~ = type_name; <>

let type_binding := id = Iden; typ = type_annotation; { (($startpos, $endpos), id, typ) }

let type_signature == Arrow; ~ = type_name; <>


// Statements

// TODO match

let stmt :=
  | expr_stmt
  | compound_stmt
  | iteration_stmt
  | declaration_stmt
  | selection_stmt
  | jump_stmt
  | assignment_stmt

let jump_stmt :=
  | Return; e = option(expr); Semi; { Return (($startpos, $endpos), e) }
  | Break; Semi; { Break (($startpos, $endpos)) }
  | Continue; Semi; { Continue (($startpos, $endpos)) }

let compound_stmt := LBrace; stmts = list(stmt); RBrace; { Stmt.Block (($startpos, $endpos), stmts) }

let expr_stmt := e = expr; Semi; { Stmt.Expr (($startpos, $endpos), e) }

let declaration_stmt :=
  | Let; mut = option(Mut); id = Iden; annotation = option(type_annotation); Assign; e = expr; Semi;
    { Stmt.Declaration {span=($startpos, $endpos); mut=(match mut with Some _ -> Mut | None -> Const); id=id; type_annotation=annotation; defn=Some e} }
  | Let; mut = option(Mut); id = Iden; annotation = option(type_annotation); Semi;
    { Stmt.Declaration {span=($startpos, $endpos); mut=(match mut with Some _ -> Mut | None -> Const); id=id; type_annotation=annotation; defn=None} }

let assignment_stmt := e1 = expr; Assign; e2 = expr; Semi; { Stmt.Assignment (($startpos, $endpos), e1, e2) }

let selection_stmt :=
  | If; LParen; e = expr; RParen; body = compound_stmt; { Stmt.If (($startpos, $endpos), e, body, None) }
  | If; LParen; e = expr; RParen; body = compound_stmt; Else; els = else_stmt; { Stmt.If (($startpos, $endpos), e, body, Some els) }

let else_stmt :=
  | compound_stmt
  | selection_stmt

let iteration_stmt :=
  | for_stmt
  | while_stmt

let for_stmt := For; LParen; assign = declaration_stmt; cond = expr; Semi; post = expr; RParen; body = compound_stmt; { Stmt.For (($startpos, $endpos), assign, cond, post, body) }

let while_stmt := While; LParen; cond = expr; RParen; body = compound_stmt;
  { Stmt.While (($startpos, $endpos), cond, body) }


////////// Expressions

let primary_expr :=
  | i = Iden; { Var (($startpos, $endpos), i) }
  | lit = Literal; { Literal (($startpos, $endpos), lit) }
  | LParen; ~ = expr; RParen; <>
  (* parens to appease the parser gods is a bit nasty, fix *)
  | LParen; expr = expr; As; typ = type_name; RParen; { Cast (($startpos, $endpos), expr, typ) }
  | id = Iden; LParen; args = separated_list(Comma, expr); RParen; { Call (($startpos, $endpos), id, args) }
  | id = Iden; LBrace; inits = separated_list(Comma, expr); RBrace; { Initializer (($startpos, $endpos), id, inits) }
  | id = Iden; LSquare; inner = expr; RSquare;
    { Expr.PostFix(($startpos, $endpos), Expr.Binary (($startpos, $endpos), Var (($startpos, $endpos), id), Plus, inner), Deref) }

let postfix_expr :=
  | primary_expr
  | expr = postfix_expr; op = postfix_op; { PostFix (($startpos, $endpos), expr, op) }
  | expr = postfix_expr; Dot; id = Iden; { FieldAccess (($startpos, $endpos), expr, id) }

let unary_expr :=
  | postfix_expr
  | op = unary_op; e = unary_expr; { Unary (($startpos, $endpos), op, e) }
  | Sizeof; LParen; t = type_name; RParen; { Sizeof (($startpos, $endpos), t) }

let multiplicative_expr :=
  | unary_expr
  | e1 = multiplicative_expr; op = multiplicative_op; e2 = unary_expr; { Expr.Binary (($startpos, $endpos), e1, op, e2) }

let additive_expr :=
  | multiplicative_expr
  | e1 = additive_expr; op = additive_op; e2 = multiplicative_expr; { Expr.Binary (($startpos, $endpos), e1, op, e2) }

let shift_expr :=
  | additive_expr
  | e1 = shift_expr; op = shift_op; e2 = additive_expr; { Expr.Binary (($startpos, $endpos), e1, op, e2) }

let relation_expr :=
  | shift_expr
  | e1 = relation_expr; op = relational_op; e2 = shift_expr; { Expr.Binary (($startpos, $endpos), e1, op, e2) }

let equality_expr :=
  | relation_expr
  | e1 = equality_expr; op = equality_op; e2 = relation_expr; { Expr.Binary (($startpos, $endpos), e1, op, e2) }

let and_expr :=
  | equality_expr
  | e1 = and_expr; And; e2 = equality_expr; { Expr.Binary (($startpos, $endpos), e1, And, e2) }

let xor_expr :=
  | and_expr
  | e1 = xor_expr; Xor; e2 = and_expr; { Expr.Binary (($startpos, $endpos), e1, Xor, e2) }

let or_expr :=
  | xor_expr
  | e1 = or_expr; Or; e2 = xor_expr; { Expr.Binary (($startpos, $endpos), e1, Or, e2) }

let land_expr :=
  | or_expr
  | e1 = land_expr; LAnd; e2 = or_expr; { Expr.Binary (($startpos, $endpos), e1, LAnd, e2) }

let lor_expr :=
  | land_expr
  | e1 = lor_expr; LOr; e2 = land_expr; { Expr.Binary (($startpos, $endpos), e1, LOr, e2) }

let expr == lor_expr

let unary_op ==
  | Minus; { Ast.Operator.Neg }
  | Bang; { Ast.Operator.Bang }
  | Tilde; { Ast.Operator.Tilde }
  | Ampersand; { Ast.Operator.Addr }

let multiplicative_op ==
  | Star; { Ast.Operator.Star }
  | Slash; { Ast.Operator.Slash }
  | Mod; { Ast.Operator.Mod }

let additive_op ==
  | Plus; { Ast.Operator.Plus }
  | Minus; { Ast.Operator.Minus }

let relational_op ==
  | Gt; { Ast.Operator.Gt }
  | Gte; { Ast.Operator.Gte }
  | Lt; { Ast.Operator.Lt }
  | Lte; { Ast.Operator.Lte }

let equality_op ==
  | Eq; { Ast.Operator.Eq }
  | Neq; { Ast.Operator.Neq }

let shift_op ==
  | LShift; { Ast.Operator.LShift }
  | RShift; { Ast.Operator.RShift }

let postfix_op ==
  | Incr; { Ast.Operator.Incr }
  | Decr; { Ast.Operator.Decr }
  | Caret; { Ast.Operator.Deref }
