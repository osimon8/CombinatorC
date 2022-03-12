open Utils
open Parse
open Ast
open Encode
open Compiler

let () =
 let ast = parse("A / -345") in 
  print_endline (string_of_bexp ast);
let entities = compile_bexp_to_json ast in 
let js_string = to_json_string (json_output "test" entities) in
let blueprint = encode "test" entities in 
prerr_endline(js_string ^ "\n" ^ blueprint)


