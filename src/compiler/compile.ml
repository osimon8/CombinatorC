open Circuit
open Combinator
open Optimize
open Layout
open Utils
open Ast
open Directive

let json_of_symbol s = 
  `Assoc [("type", `String "virtual"); ("name", `String ("signal-" ^ s))]

let json_of_value v =
  `Intlit (Int32.to_string v)

let json_of_config (cfg:cfg) : string * json = 
  let mk_sig pre s = (pre ^ "_signal", json_of_symbol s) in

  let parse_ao (pre:string) (o:aop) = 
    begin match o with 
    | Symbol s -> mk_sig pre s
    | Const c -> (pre ^ "_constant", json_of_value c)
    | Each -> mk_sig pre "each"
    end 
  in 

  let parse_do (pre:string) (o:dop) i = 
    begin match o with 
    | Const c -> if i <> 1 then failwith "illegal argument to decider combinator"
                else ("constant", json_of_value c)
    | Symbol s -> mk_sig pre s
    | Each -> mk_sig pre "each"
    | Anything -> mk_sig pre "anything"
    | Everything -> mk_sig pre "everything"
    end
  in 

  let c_map (i:int) (data:data) =  
    let s, v = data in
    `Assoc [("signal", json_of_symbol s); 
            ("count", json_of_value v); 
            ("index", `Int (i + 1))] in

  ("control_behavior", `Assoc (
  begin match cfg with 
  | A (o1, op, o2, out) ->  [("arithmetic_conditions", `Assoc
                                [(parse_ao "first" o1); (parse_ao "second" o2); 
                                ("operation", `String (string_of_arithmetic_op op)); 
                                (parse_ao "output" out)] 
                              )]
  | D (o1, op, o2, out, t) -> [("decider_conditions", `Assoc
                                ([(parse_do "first" o1 0); (parse_do "second" o2 1); 
                                ("comparator", `String (string_of_decider_op op)); 
                                (parse_do "output" out 2)]  
                                @ (begin match t with 
                                    | One -> [("copy_count_from_input", `Bool false)]
                                    | InpCount -> []
                                    end))
                              )] 
  | C cfg -> [("filters", `List (List.mapi c_map cfg))]
  end))

let json_of_conn = 
  fun (clist:CG.edge list) : (string * json) list ->  
    let red, green = List.partition (fun (_, color, _) -> 
                                      begin match color with 
                                      | Red -> true
                                      | Green -> false 
                                      end) clist in 


    let map_conns l = List.map (fun (_, _, c) -> `Assoc [("entity_id", `Int (id_of_conn c));
                                                        ("circuit_id", `Int (type_id_of_conn c))]) l in 


    let rl, gl = List.length red, List.length green in 
    
    begin match rl, gl with 
    | 0, 0 ->  []
    | _, 0 ->  [(string_of_wire_color Red, `List (map_conns red)) ] 
    | 0, _ ->  [(string_of_wire_color Green, `List (map_conns green)) ] 
    | _, _ ->  [(string_of_wire_color Red, `List (map_conns red));
                     (string_of_wire_color Green, `List (map_conns green)) ] 
    end 

let json_of_combinator (c: combinator) (g: connection_graph) (p:placement) : json = 
  let id, name, cfg_json = begin match c with
 | Arithmetic (id, cfg) -> id, "arithmetic-combinator", [json_of_config (A cfg)]
 | Decider (id, cfg) ->  id, "decider-combinator", [json_of_config (D cfg)]
 | Constant (id, cfg) -> id, "constant-combinator", [json_of_config (C cfg)]
 | Pole id -> id, "substation", [] (* small-electric-pole*)
  end in 

  let joc label clist = 
    begin match json_of_conn clist with 
    | [] -> [] 
    | l -> [(label, `Assoc l)]
  end in 

  let conns = [("connections", begin match c with 
 | Arithmetic _ -> `Assoc (joc "1" (succs g (Ain id)) @ joc "2" (succs g (Aout id)))
 | Decider _ ->  `Assoc (joc "1" (succs g (Din id)) @ joc "2" (succs g (Dout id)))
 | Constant _-> `Assoc (joc "1" (succs g (C id)))
 | Pole _ -> `Assoc (joc "1" (succs g (P id)))
  end 
  )] in 

  let x, y = p in
  `Assoc ([("entity_number", `Int id); 
          ("name", `String name);
          ("position", `Assoc [("x", `Float x); ("y", `Float y)]);
          ] @ cfg_json @ conns)

 let json_of_circuit (circuit: concrete_circuit) : json list = 
  let (combs, g, _), (_, _, placements) = circuit in 

  let zipped = List.combine combs placements in
  let json_list = List.map (fun (c, p) -> json_of_combinator c g p) zipped in
  json_list

let json_of_circuits (circuits: circuit list) : json list = 
  let layouts = layout_circuits circuits in 
  (* print_endline ("INITED CTR AT " ^ (string_of_int mid + 1)); *)

  let concrete_circuits = List.combine circuits layouts  in
  (* List.iter print_endline @@ List.map string_of_layout layouts; *)
  
  let wrapped = List.map wrap_io concrete_circuits in 
  (* List.iter (fun (c, l) -> 
    print_endline (string_of_layout l);
    let combs, g, _ = c in 
    List.iter print_endline @@ List.map string_of_combinator combs;
    print_edges g;

    
    ) wrapped; *)
  List.concat_map json_of_circuit wrapped 

let compile_bexp_to_circuit ?optimize:(optimize=true) (o_sig:symbol) (b: bexp) : circuit = 
  let circuit = circuit_of_bexp o_sig b in 
    if optimize then 
      let circuit_opt = primitive_optimization circuit in
      (* let circuit_opt = remap_ids ctr circuit_opt in *)
      (* wrap_io ctr circuit_opt  *)
      circuit_opt
    else 
      circuit 

let json_of_bexp ?optimize:(optimize=true) (o_sig:symbol) (b: bexp) : json list = 
  let circuit = compile_bexp_to_circuit ~optimize o_sig b in 
  json_of_circuits [circuit]