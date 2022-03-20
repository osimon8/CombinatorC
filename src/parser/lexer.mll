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

  let unexpected_char lexbuf (c:char) (d:int) : 'a =
    raise (Lexer_error (
        Printf.sprintf "Unexpected character at position %d: '%c'" d c))
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
let whitespace = ['\t' ' ' '\r']
let circuit_bind = "circuit"(whitespace+)
let newline = '\n' | "\r\n" | eof
let comment = "//"[^'\r''\n']*newline
let directive = '#'

rule token = parse
  | eof         { EOF }
  | whitespace+ { token lexbuf }  (* skip whitespace *)
  | comment     
  | '\n'        { MenhirLib.LexerUtil.newline lexbuf; token lexbuf }
  | signal { VAR (lexeme lexbuf) }
  | num    { LIT (Int32.of_string (lexeme lexbuf)) }
  | circuit_bind { CIRCUIT_BIND }
  | directive   { DIRECTIVE }
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
  | "true"      { LIT 1l }
  | "false"     { LIT 0l }
  | ';'         { SEMI }
  | '='         { ASSIGN }
  | "if"        { IF } 
  | "then"      { THEN }
  | "else"      { ELSE }
  (* | '?'         { QUESTION }
  | ':'         { COLON } *)
  | "??"        { COALESCE }
  | "==="       { LEQ }
  | "!=="       { LNEQ }
  | "\\/"       { UNION }
  | "@"         { CONCAT }
  | single_case_word  { WORD (lexeme lexbuf) }
  | _ as c      { unexpected_char lexbuf c (lexeme_start lexbuf) }
