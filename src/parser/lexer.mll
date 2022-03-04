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
let identifer = (character | ichar) (character | ichar | digit)*
let signal = uppercase
let whitespace = ['\t' ' ' '\r' '\n']

(* Returns a token of type as specified in parser.mly 
   
   Each token carries a Range.t value indicating its
   position in the file.
*)
rule token = parse
  | eof         { EOF }
  | whitespace+ { token lexbuf }  (* skip whitespace *)
  | signal { VAR (lexeme lexbuf) }
  | digit+    { LIT (int_of_string (lexeme lexbuf)) }
  | '+'         { PLUS }
  | _ as c      { unexpected_char lexbuf c }
