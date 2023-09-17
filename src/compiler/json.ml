open Ast.Circuit
open Ast.Combinator
open Optimize
open Layout
open Utils
open Ast.Expression

let virt = Str.regexp "signal-(.+)" 
let fluid = Str.regexp "fluid-(.+)" 
let item = Str.regexp "item-(.+)" 

let sig_regex = Str.regexp "\\([a-z]+\\)-\\(.+\\)"
let json_of_symbol s =
  let err () = (prerr_endline @@ Printf.sprintf "Invalid signal \"%s\", this error shouldn't happen" s; exit 1) in 
  if not (Str.string_match sig_regex s 0) then err (); 
  let ty = Str.matched_group 1 s in
  let ty = if ty = "signal" then "virtual" else ty in   
  let n = if ty = "virtual" then s else Str.matched_group 2 s in 

  `Assoc [("type", `String ty); ("name", `String n)]

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

  let parse_lo (pre:string) (o:dop) i = 
    begin match o with 
    | Const c -> if i <> 1 then failwith "illegal argument to lamp"
                else ("constant", json_of_value c)
    | Symbol s -> mk_sig pre s
    | Each -> failwith "illegal argument to lamp"
    | Anything -> mk_sig pre "anything"
    | Everything -> mk_sig pre "everything"
    end in

  let c_map (i:int) (data:data) =  
    let s, v = data in
    `Assoc [("signal", json_of_symbol s); 
            ("count", json_of_value v); 
            ("index", `Int (i + 1))] in

  ("control_behavior", `Assoc (
  begin match cfg with 
  | A cfg ->  [("arithmetic_conditions", `Assoc
                                [(parse_ao "first" cfg.left_input); (parse_ao "second" cfg.right_input); 
                                ("operation", `String (string_of_arithmetic_op cfg.op)); 
                                (parse_ao "output" cfg.output)] 
                              )]
  | D cfg -> [("decider_conditions", `Assoc
                                ([(parse_do "first" cfg.left_input 0); (parse_do "second" cfg.right_input 1); 
                                ("comparator", `String (string_of_decider_op cfg.op)); 
                                (parse_do "output" cfg.output 2)]  
                                @ (begin match cfg.output_type with 
                                    | One -> [("copy_count_from_input", `Bool false)]
                                    | InpCount -> []
                                    end))
                              )] 
  | C cfg -> [("filters", `List (List.mapi c_map cfg))]
  | L cfg -> [("circuit_condition", `Assoc ([(parse_lo "first" cfg.left_input 0); 
                                                      (parse_lo "second" cfg.right_input 1); 
                                                      ("comparator", `String (string_of_decider_op cfg.op))]))]
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
 | Lamp (id, cfg) -> id, "small-lamp", [json_of_config (L cfg)] 
 | Pole (id, t) -> id, 
    begin match t with 
    | Small -> "small-electric-pole"
    | Medium -> "medium-electric-pole"
    | Big -> "big-electric-pole"
    | Substation -> "substation"
    end, [] 
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
 | Lamp _ -> `Assoc (joc "1" (succs g (L id)))
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
  let concrete_circuits = List.combine circuits layouts  in
  let wrapped = List.map wrap_io concrete_circuits in 
  let remapped = remap_ids_concrete wrapped in 
  List.concat_map json_of_circuit remapped 

let json_of_compiled_circuits (circuits: compiled_circuit list) : json list = 
  let abstract, concrete = List.partition (fun c -> match c with | Abstract _ -> true | _ -> false) circuits in 
  let abstract = List.map (fun c -> match c with | Abstract c -> c | _ -> failwith "impossible") abstract in 
  let layouts = layout_circuits abstract in 
  let concrete_layouts = List.map (fun c -> match c with | Concrete c -> c | _ -> failwith "impossible") concrete in 
  let concrete_circuits = List.combine abstract layouts @ concrete_layouts  in
  let wrapped = List.map wrap_io concrete_circuits in 
  let remapped = remap_ids_concrete wrapped in 
  List.concat_map json_of_circuit remapped 
