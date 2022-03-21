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
  let reserved_words = [
  ( "output", OUTPUT);
  ("if", IF);
  ("then", THEN);
  ("else", ELSE);
  ("true", LIT 1l);
  ("false", LIT 0l);
  ( "circuit", CIRCUIT_BIND);

  ("#", DIRECTIVE);
  ( ";", SEMI);
  ( ":", COLON);
  ( "+", PLUS);
  ( "-", MINUS);
  ( "*", MUL);
  ( "/", DIV);
  ( "%", MOD);
  ( "*", MUL);
  ( "=", ASSIGN);
  ( "==", EQ);
  ( "!", NOT);
  ( "!=", NEQ);
  ( "!==", LNEQ);
  ( "===", LEQ);
  ( "&", AND);
  ( "|", OR);
  ( "^", XOR);
  ( "&&", LAND);
  ( "||", LOR);
  ( ">", GT);
  ( ">=", GTE);
  ( "<", LT);
  ( "<=", LTE);
  ( "<<", LSHIFT);
  ( ">>", RSHIFT);
  ( "(", LPAREN);
  ( ")", RPAREN);
  ( "\\/", UNION);
  ( "@", CONCAT)

  ]

  let (symbol_table : (string, Parser.token) Hashtbl.t) = Hashtbl.create 256
  let _ =
    List.iter (fun (str,t) -> Hashtbl.add symbol_table str t) reserved_words

  let token_lookup lexbuf =
    let str = lexeme lexbuf in 
    try (Hashtbl.find symbol_table str) 
    with _ -> WORD str

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
let ident_symbol = character | ichar
let ident = (ident_symbol)(ident_symbol)+ | lowercase
let whitespace = ['\t' ' ' '\r']
let circuit_bind = "circuit"(whitespace+)
let newline = '\n' | "\r\n" | eof
let comment = "//"[^'\r''\n']*newline

rule token = parse
  | eof         { EOF }
  | whitespace+ { token lexbuf }  (* skip whitespace *)
  | comment     
  | '\n'        { MenhirLib.LexerUtil.newline lexbuf; token lexbuf }
  | signal { VAR (lexeme lexbuf) }
  | num    { LIT (Int32.of_string (lexeme lexbuf)) }
  | ident   { token_lookup lexbuf }
  | '#'  
  | '+'        
  | '-'        
  | '*'        
  | '/'        
  | '%'        
  | "<<"       
  | ">>"       
  | '&'        
  | '|'        
  | '^'        
  | "**"       
  | '<'        
  | '>'        
  | ">="       
  | "<="       
  | "=="       
  | "!="       
  | '('        
  | ')'        
  | '!'        
  | "||"       
  | "&&"        
  | ';'  
  | ':'      
  | '='        
  | "??"       
  | "==="       
  | "!=="       
  | "\\/"       
  | "@"         { token_lookup lexbuf }
  | _ as c      { unexpected_char lexbuf c (lexeme_start lexbuf) }
