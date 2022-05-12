{
  open Lexing
  open Parser


  let incr_linenum lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_bol = lexbuf.lex_curr_pos;
      pos_lnum = pos.pos_lnum + 1
    }
}

let non_zero_digit = ['1'-'9']
let digit = '0' | non_zero_digit
let int = '0' | non_zero_digit digit*

let alpha = ['a'-'z' 'A'-'Z']
let identifier = alpha (alpha|digit|'_')*

let whitespace = [' ' '\t']+
(* TODO maybe we should handle windows stuff too *)
let newline = '\n'

rule token = parse
  | int { Literal (Int (int_of_string (Lexing.lexeme lexbuf))) }
  | '{' { LBrace }
  | '}' { RBrace }
  | '(' { LParen }
  | ')' { RParen }
  | ';' { Semi }
  | '!' { Bang }
  | '-' { Minus }
  | '~' { Tilde }
  | '+' { Plus }
  | '*' { Star }
  | '/' { Slash }
  | "&&" { LAnd }
  | "||" { LOr }
  | "==" { Eq }
  | "!=" { Neq }
  | '>' { Gt }
  | ">=" { Gte }
  | '<' { Lt }
  | "<=" { Lte }
  | '=' { Assign }
  | "if" { If }
  | "else" { Else }
  | ":" { Colon }
  | "return" { Return }
  | "int" { Int }
  | "for" { For }
  | "while" { While }
  | "do" { Do }
  | "break" { Break }
  | "continue" { Continue }
  | identifier { Iden (Lexing.lexeme lexbuf) }
  | whitespace { token lexbuf }
  | newline { incr_linenum lexbuf; token lexbuf }
  | eof { Eof }
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }
