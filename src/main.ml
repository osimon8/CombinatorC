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

let usage_msg = "main [--output-json] <file1>"
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
    | None -> failwith ("Missing input file, usage: " ^ usage_msg)
    end in

  let code = 
  try 
    Core.In_channel.read_all name
  with Sys_error s -> failwith s 
  in

  let directives, assignment_list = parse code in 
  let cfg = config_of_directives directives in 
  set_config cfg;

  let f i (o_sig, ast) =
      let ast = if optimize_b then optimize_bexp ast else ast in 
      (* print_endline (string_of_bexp ast); *)
      let c = compile_bexp_to_circuit ~optimize ~i o_sig ast in
      let _, _, _, m = c in 
      let m_id, _, _, _, _ = m in 
      m_id + 1, c
  in

  let _, circuits = List.fold_left_map f 1 assignment_list in 
  let entities = json_of_circuits circuits in 

  let js_string = to_json_string (json_output name entities) in
  if !output_json then 
    print_endline js_string
   else 
    let blueprint = encode name entities in 
    print_endline blueprint


