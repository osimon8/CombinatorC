open Utils
open Parse
open Ast
open Encode
open Compiler

let optimize = true

let () =
  let ast = parse("A * 2**(4 * -1 - -8) / ((4 + B) + 3)") in 
  let ast = if optimize then optimize_bexp ast else ast in 
  print_endline (string_of_bexp ast);

  let entities = compile_bexp_to_json ~optimize:optimize ast in 
  let js_string = to_json_string (json_output "test" entities) in
  let blueprint = encode "test" entities in 
  print_endline(js_string ^ "\n" ^ blueprint)


