{
  open Lexing
  open Parser

  exception Lexer_error of string

  let unexpected_char lexbuf (c:char) : 'a =
    let p = lexeme_start_p lexbuf in
    let o = p.pos_cnum - p.pos_bol in 
    let l = p.pos_lnum in 
    raise (Lexer_error (
        Printf.sprintf "Unexpected character at line %d, character %d: '%c'" l o c))
  let reserved_words = [
  ("output", OUTPUT);
  ("if", IF);
  ("then", THEN);
  ("else", ELSE);
  ("true", LIT 1l);
  ("false", LIT 0l);
  ("concrete", CONCRETE);
  ("circuit", CIRCUIT_BIND);
  ("at", AT);

  ( ";", SEMI);
  ( ",", COMMA);
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
    with _ -> IDENT str

  let direct = ref ""
  let direct_state = ref 0

  let directive_error () = raise (Lexer_error "Syntax Error: Directive syntax is \"#<DIRECTIVE> <ARGUMENT>\"")

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
let mixed_case_word = (ident_symbol)(ident_symbol)+
let ident = mixed_case_word | lowercase
let whitespace = ['\t' ' ' '\r']
let circuit_bind = "circuit"(whitespace+)
let newline = '\n' | "\r\n" | eof
let comment = "//"[^'\n']*newline

rule token = parse
  | eof         { EOF }
  | whitespace+ { token lexbuf }  (* skip whitespace *)
  | "/*"        { comments lexbuf }
  | comment     
  | '\n'        { MenhirLib.LexerUtil.newline lexbuf; token lexbuf }
  | signal { VAR (lexeme lexbuf) }
  | num    { LIT (Int32.of_string (lexeme lexbuf)) }
  | ident   { token_lookup lexbuf }
  | '#'     { let p = lexeme_start_p lexbuf in
              if p.pos_cnum - p.pos_bol = 0 then (direct_state := 0; directive lexbuf) 
              else raise (Lexer_error ("Syntax Error: Directives must start at the beginning of a line"))}
  | '+'        
  | '-'        
  | '*'        
  | '/'        
  | '%'        
  | "<<"       
  | ">>"             
  | '^'        
  | "**"    
  | "==="       
  | "!=="              
  | ">="       
  | "<="       
  | "=="       
  | "!="  
  | '<'        
  | '>'       
  | '('        
  | ')'         
  | "||"       
  | "&&"      
  | '&'        
  | '|'    
  | ';'  
  | ':'              
  | "??"       
  | '!'  
  | ','
  | '='
  | "\\/"       
  | "@"         { token_lookup lexbuf }
  | _ as c      {  unexpected_char lexbuf c }

and directive = parse 
  | whitespace+       { if !direct_state = 0 then (directive_error ())  else directive lexbuf}
  | single_case_word  { let s = lexeme lexbuf in
                        if !direct_state = 0 then 
                        (direct := s; direct_state := 1; directive lexbuf)
                         else if !direct_state = 1 then
                          (direct_state := 2; DIRECTIVE(!direct, s)) 
                          else (directive_error ())}
  | mixed_case_word   { raise (Lexer_error ("Directive or directive argument: \"" ^ (lexeme lexbuf) ^ "\" must be either all uppercase or all lowercase")) }
  | _ as c      {  unexpected_char lexbuf c }

and comments = parse 
  | "*/"       { token lexbuf }
  | [^ '\n']   { comments lexbuf }
  | "\n"       { MenhirLib.LexerUtil.newline lexbuf; comments lexbuf }
  | eof        { raise (Lexer_error "Unclosed multiline comment")}