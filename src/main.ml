open Utils
open Parse
open Ast
open Encode
open Compiler.Compile
open Compiler.Config

let optimize = true

(* WARNING: If this is set to false, some expressions 
  using comparison operations may not compile *)
let optimize_b = true

let usage_msg = "combc [--output-json] <file>"
let input_file = ref None
let output_json = ref false 
let speclist = [("--output-json", Arg.Set output_json, "Output json instead of blueprint string")] 

let arg_fun s = 
  input_file := Some s

let () =
  Arg.parse speclist arg_fun usage_msg;

  let name = 
    begin match !input_file with 
    | Some s -> s
    | None -> prerr_endline ("Missing input file, usage:\n" ^ usage_msg); exit 1;
    end in

  let directives, assignment_list = parse name in  
  let cfg = config_of_directives directives in 
  set_config cfg;

  let f (o_sig, ast) =
      let ast = if optimize_b then optimize_bexp ast else ast in 
      (* print_endline (string_of_bexp ast); *)
      let c = compile_bexp_to_circuit ~optimize o_sig ast in
      c
  in

  let circuits = List.map f assignment_list in 
  let entities = json_of_circuits circuits in 

  let js_string = to_json_string (json_output name entities) in
  if !output_json then 
    print_endline js_string
   else 
    let blueprint = encode name entities in 
    print_endline blueprint


