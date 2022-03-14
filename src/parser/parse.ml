open Ast
open Compiler.Directive
module P = Parser

let parse (input: string) : directive list * assignment list =
  try
    (* Lexer.reset_lexbuf filename buf ; *)
    P.toplevel Lexer.token (Lexing.from_string input)
  with P.Error ->
    failwith
      (Printf.sprintf "Parse error")