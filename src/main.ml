open Utils
open Parse
open Ast
open Encode
open Compiler

let optimize = true

(* WARNING: If this is set to false, some expressions 
  using comparison operations may not compile *)
let optimize_b = true

let () =
  let ast = parse("A == (B + 4 * (6 + C % 10))") in 
  let ast = if optimize_b then optimize_bexp ast else ast in 
  print_endline (string_of_bexp ast);

  let entities = compile_bexp_to_json ~optimize:optimize ast in 
  let js_string = to_json_string (json_output "test" entities) in
  let blueprint = encode "test" entities in 
  print_endline(js_string ^ "\n" ^ blueprint)


