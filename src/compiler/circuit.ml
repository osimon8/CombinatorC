open Combinator
open Utils

type wire = 
 | Red of signal 
 | Green of signal

type connection = 
  | Ain of id
  | Aout of id
  | Din of id
  | Dout of id
  | C of id
  | P of id

module Node = struct                                                                
   type t = connection                                                                     
   let compare = Stdlib.compare                                                 
   let hash = Hashtbl.hash                                                          
   let equal = (=)                                                                  
end                                                                                 

module CG = Graph.Imperative.Graph.Concrete(Node)

type connection_graph = CG.t

type circuit = wire * combinator list * connection_graph

let id_of_conn conn = 
  begin match conn with 
  | Ain id
  | Aout id
  | Din id
  | Dout id
  | C id
  | P id -> id
end

(* id used by Factorio in circuit id in JSON representation *)
let type_id_of_conn conn = 
  begin match conn with 
  | Ain _
  | Din _
  | C _
  | P _ -> 1
  | Aout _
  | Dout _ -> 2
end

let string_of_conn conn = 
  begin match conn with 
  | Ain id -> "(Ain " ^ string_of_int id ^ ")"
  | Aout id -> "(Aout " ^ string_of_int id ^ ")"
  | Din id -> "(Din " ^ string_of_int id ^ ")"
  | Dout id -> "(Dout " ^ string_of_int id ^ ")"
  | C id -> "(C " ^ string_of_int id ^ ")"
  | P id -> "(P " ^ string_of_int id ^ ")"
end

let print_edges (g:connection_graph) : unit = 
  print_endline("PRINTING GRAPH");
  CG.iter_edges (fun v1 v2 -> print_endline (string_of_conn v1 ^ " <-> " ^ string_of_conn v2)) g;
  print_endline("--------------------")

let json_of_symbol s = 
  `Assoc [("type", `String "virtual"); ("name", `String ("signal-" ^ s))]

let json_of_config (cfg:cfg) : string * json = 
  let parse_ao (pre:string) (o:aop) = 
    begin match o with 
    | Symbol s -> (pre ^ "_signal", json_of_symbol s)
    | Const c -> (pre ^ "_constant", `Int c)
    | Each -> failwith "unsupported"
    end 
  in 

  let parse_do (pre:string) (o:dop) i = 
    if i <> 1 then 
      begin match o with 
      | Symbol s -> (pre ^ "_signal", json_of_symbol s)
      | _ -> failwith "unsupported"
      end
    else 
      begin match o with 
      | Symbol s -> (pre ^ "_signal", json_of_symbol s)
      | Const c -> ("constant", `Int c)
      | _ -> failwith "unsupported"
      end 
  in 

  let c_map (i:int) (data:data) =  
    let s, v = data in
    (* let s : string = begin match s with Red s | Green s -> s end in *)
    `Assoc [("signal", json_of_symbol s); 
            ("count", `Int v); 
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

let succs g id = 
  if CG.mem_vertex g id then CG.succ g id else []

let json_of_conn color id = 
  fun (clist:connection list) : json ->  
     `Assoc [(color, `List (List.map (fun c -> `Assoc [("entity_id", `Int (id_of_conn c));
                                                        ("circuit_id", `Int (type_id_of_conn c))])
      clist))]

let json_of_combinator (c: combinator) (wire: wire) (g: connection_graph) (cid:id) : json = 
  let id, name, cfg_json = begin match c with
 | Arithmetic (id, cfg) -> id, "arithmetic-combinator", [json_of_config (A cfg)]
 | Decider (id, cfg) ->  id, "decider-combinator", [json_of_config (D cfg)]
 | Constant (id, cfg) -> id, "constant-combinator", [json_of_config (C cfg)]
 | Pole id -> id, "small-electric-pole", []
  end in 

  let color = begin match wire with 
  | Red _ -> "red"
  | Green _ -> "green"
  end in

  let joc = json_of_conn color id in

  let conns = [("connections", begin match c with 
 | Arithmetic _ -> `Assoc [("1", joc (succs g (Ain id))); ("2", joc (succs g (Aout id)))]
 | Decider _ ->  `Assoc [("1", joc (succs g (Din id))); ("2", joc (succs g (Dout id)))]
 | Constant _-> `Assoc [("1", joc (succs g (C id)))]
 | Pole _ -> `Assoc [("1", joc (succs g (P id)))]
  end 
  )] in 

  `Assoc ([("entity_number", `Int id); 
          ("name", `String name);
          ("position", `Assoc [("x", `Int (id * 1)); ("y", `Int 0)]);
          ] @ cfg_json @ conns)

 let json_of_circuit (id: id) (circuit: circuit) : json list = 
  let wire, combs, g = circuit in 
  let json_list = List.map (fun c -> json_of_combinator c wire g id) combs in
  json_list


let json_of_circuits (circuits: circuit list) : json list = 
  let red, green = List.partition (fun (w, _, _) -> begin match w with
                                  | Red _ -> true | _ -> false end) circuits in
  let rm, gm = List.mapi json_of_circuit red, List.mapi json_of_circuit green in
  List.flatten (rm @ gm)