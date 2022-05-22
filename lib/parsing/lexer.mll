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

let escape_sequence =
  '\\' ['\''  '\"'  '?'  '\\'  'n'  'r'  't' ]
let alpha = ['a'-'z' 'A'-'Z']
let identifier = (alpha|'_') (alpha|digit|'_')*

let whitespace = [' ' '\t']+
(* TODO maybe we should handle windows stuff too *)
let newline = '\n'

rule token = parse

  (* Keywords *)
  | "return" { Return }
  | "if" { If }
  | "else" { Else }
  | "for" { For }
  | "while" { While }
  | "break" { Break }
  | "continue" { Continue }
  | "fn" { Fn }
  | "let" { Let }
  | "mut" { Mut }
  | "sizeof" { Sizeof }
  | "as" { As }
  | "null" { Literal Null }

  | "struct" { Struct }

  | "u8" { U8 }
  | "u32" { U32 }
  | "bool" { Bool }

  (* Punc *)
  | '{' { LBrace }
  | '}' { RBrace }
  | '(' { LParen }
  | ')' { RParen }
  | '[' { LSquare }
  | ']' { RSquare }
  | ';' { Semi }
  | ':' { Colon }
  | "->" { Arrow }
  | ',' { Comma }

  (* Operators *)
  | '&' { Ampersand }
  | '^' { Caret }
  | "." { Dot }

  | '-' { Minus }
  | '!' { Bang }
  | '~' { Tilde }
  | '*' { Star }
  | '/' { Slash }
  | '%' { Mod }
  | '+' { Plus }
  | "++" { Incr }
  | "--" { Decr }

  | "<<" { LShift }
  | ">>" { RShift }
  | "&" { And }
  | "|" { Or }
  | "^" { Xor }

  | "&&" { LAnd }
  | "||" { LOr }

  | "==" { Eq }
  | "!=" { Neq }
  | '>' { Gt }
  | ">=" { Gte }
  | '<' { Lt }
  | "<=" { Lte }

  | '=' { Assign }

  (* Literals *)
  | "true" { Literal (Bool true) }
  | "false" { Literal (Bool false) }
  | int { Literal (Num (int_of_string (Lexing.lexeme lexbuf))) }
  | '\'' (alpha | digit | escape_sequence) '\''
    {
      let ch = Lexing.sub_lexeme lexbuf ((Lexing.lexeme_start lexbuf) + 1) ((Lexing.lexeme_end lexbuf) - 1) in
      Literal (Char (Scanf.unescaped ch).[0] )
    }
  | '"' { str (Buffer.create 32) lexbuf }
  | identifier { Iden (Lexing.lexeme lexbuf) }

  (* Other *)
  | whitespace { token lexbuf }
  | newline { incr_linenum lexbuf; token lexbuf }
  | "//" { single_line_comment lexbuf }
  | "/*" { multi_line_comment lexbuf }
  | eof { Eof }
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }

and single_line_comment = parse
  | newline { incr_linenum lexbuf; token lexbuf }
  | eof { Eof }
  | _ { single_line_comment lexbuf }

and multi_line_comment = parse
  | "*/" { token lexbuf }
  | newline { incr_linenum lexbuf; multi_line_comment lexbuf }
  | eof { raise (Failure ("EOF in multi line comment")) }
  | _ { multi_line_comment lexbuf }

and str buf = parse
  | '"'       { Literal (String (Buffer.contents buf)) }
  | '\\' '\'' { Buffer.add_char buf '\''; str buf lexbuf }
  | '\\' '\"' { Buffer.add_char buf '\"'; str buf lexbuf }
  | '\\' '?' { Buffer.add_char buf '?'; str buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; str buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; str buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; str buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; str buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      str buf lexbuf
    }
  | _ { raise (Failure ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Failure ("String is not terminated")) }