open Utils
open Parse
open Ast
open Encode
open Compiler

let () =
 let ast = parse("4 + A") in 
  print_endline (string_of_bexp ast);
let entities = compile_bexp_to_json ast in 
(* print_endline (encode "test" entities) *)
print_endline (to_json_string (json_output "test" entities))


