open Ast.Command
open Compiler.Directive
open Printf
module L = MenhirLib.LexerUtil
module P = Parser
module E = MenhirLib.ErrorReports
module I = UnitActionsParser.MenhirInterpreter

let env checkpoint =
  match checkpoint with
  | I.HandlingError env ->
      env
  | _ ->
      assert false

let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) ->
      I.number s
  | None -> 0

let show text positions =
  E.extract text positions
  |> E.sanitize
  |> E.compress
  |> E.shorten 20 (* max width 43 *)

let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) ->
      show text (pos1, pos2)
  | None -> "???" (* impossible *)

let succeed _v =
  assert false (* impossible *)

let fail text buffer (checkpoint : _ I.checkpoint) =
  let location = L.range (E.last buffer) in
  let err = (E.show (show text) buffer) in 
  let indication = sprintf "Syntax error %s.\n" err  in
  try 
    let message = Parser_errors.message (state checkpoint) in
    let message = E.expand (get text checkpoint) message in
    eprintf "%s%s%s" location indication message;
    exit 1
  with Not_found ->
    eprintf "%s%s%s\n" location indication 
    "Unknown error, more informative error messages coming soon";
    exit 1

let slow filename text =
  let lexbuf = L.init filename (Lexing.from_string text) in
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = UnitActionsParser.Incremental.toplevel lexbuf.lex_curr_p in
  I.loop_handle succeed (fail text buffer) supplier checkpoint

type parse_result = 
  | Success of directive list * command list
  | Error of string

let fast filename : parse_result =
  let text, lexbuf =
     try 
      L.read filename 
      with Sys_error s -> prerr_endline ("File error\n" ^ s); exit 1; 
    in 
  match P.toplevel Lexer.token lexbuf  with
  | d, a ->
    Success (d, a)

  | exception DirectiveError msg -> 
    eprintf "Directive Error: %s\n" msg; exit 1 

  | exception Lexer.Lexer_error msg ->
    prerr_endline msg; exit 1
    
  | exception Parser.Error ->
    Error text


let parse (filename: string) : directive list * command list =
  (* First try fast parser, then use slow parser to generate error if fail *)
  begin match fast filename with 
  | Success (d, a) -> (d, a) 
  | Error s -> slow filename s
  end
