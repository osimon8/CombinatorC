{
  open Lexing
  open Parser

  exception Lexer_error of string

  let reset_lexbuf (filename:string) lexbuf : unit =
    lexbuf.lex_curr_p <- {
      pos_fname = filename;
      pos_cnum = 0;
      pos_bol = 0;
      pos_lnum = 1;
    }

  let unexpected_char lexbuf (c:char) : 'a =
    raise (Lexer_error (
        Printf.sprintf "Unexpected character: '%c'" c))
}

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let character = uppercase | lowercase
let ichar = ['-' '_']
let digit = ['0'-'9']
let num = '-'?digit+
let identifer = (character | ichar) (character | ichar | digit)*
let signal = uppercase
let single_case_word = (uppercase | ichar)(uppercase | ichar)+ | (lowercase | ichar)(lowercase | ichar)+
let whitespace = ['\t' ' ' '\r' '\n']
let circuit_bind = "circuit"(whitespace+)
let newline = '\n' | "\r\n" | eof
let comment = "//"[^'\r''\n']*newline
let directive = '#'

rule token = parse
  | eof         { EOF }
  | comment     
  | whitespace+ { token lexbuf }  (* skip whitespace *)
  | signal { VAR (lexeme lexbuf) }
  | num    { LIT (int_of_string (lexeme lexbuf)) }
  | circuit_bind { CIRCUIT_BIND }
  | directive   { DIRECTIVE }
  | single_case_word  { WORD (lexeme lexbuf) }
  | '+'         { PLUS }
  | '-'         { MINUS }
  | '*'         { MUL }
  | '/'         { DIV }
  | '%'         { MOD }
  | "<<"        { LSHIFT }
  | ">>"        { RSHIFT }
  | '&'         { AND }
  | '|'         { OR }
  | '^'         { XOR }
  | "**"        { EXP }
  | '<'         { LT }
  | '>'         { GT }
  | ">="        { GTE }
  | "<="        { LTE }
  | "=="        { EQ }
  | "!="        { NEQ }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '!'         { NOT }
  | "||"        { LOR }
  | "&&"        { LAND }
  | "true"      { LIT 1 }
  | "false"     { LIT 0 }
  | ';'         { SEMI }
  | '='         { ASSIGN }
  | _ as c      { unexpected_char lexbuf c }
