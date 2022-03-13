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
  let assignment_list = parse("!A + B + (2 * (C ** 4) - 3 * 2) ") in 

  let f i (o_sig, ast) =
      let ast = if optimize_b then optimize_bexp ast else ast in 
      print_endline (string_of_bexp ast);
      let c = compile_bexp_to_circuit ~optimize ~i o_sig ast in
      let _, _, _, m = c in 
      let m_id, _, _, _, _ = m in 
      m_id + 1, c
  in

  let _, circuits = List.fold_left_map f 1 assignment_list in 
  let entities = json_of_circuits circuits in 

  let js_string = to_json_string (json_output "test" entities) in
  let blueprint = encode "test" entities in 
  print_endline(js_string ^ "\n" ^ blueprint)


