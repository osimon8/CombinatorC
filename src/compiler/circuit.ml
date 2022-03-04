open Combinator

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

 (* type connection_node =  
  | CNode of connection * connection_node list *)

module Node = struct                                                                
   type t = connection                                                                     
   let compare = Stdlib.compare                                                 
   let hash = Hashtbl.hash                                                          
   let equal = (=)                                                                  
end                                                                                 

module CG = Graph.Imperative.Graph.Concrete(Node)

type connection_graph = CG.t

type circuit = wire * combinator list * connection_graph

let json_of_config (cfg:cfg) = 
  let parse_ao pre (o:aop) = 
    begin match o with 
    | Symbol s -> (pre ^ "_signal", `Assoc [ ("type", `String "virtual"); ("name", `String s) ])
    | Const c -> (pre ^ "constant", `Int c)
    | Each -> failwith "unsupported"
    end 
  in 

  ("control_behavior", `Assoc (
  begin match cfg with 
  | A (o1, op, o2, out) ->  [(parse_ao "first" o1); ("operation", `String (string_of_arithmetic_op op));
                             (parse_ao "second" o2); (parse_ao "output" out)]
  | D cfg -> []
  | C cfg -> []
  end))

let json_of_combinator (c: combinator) (wire: wire) (g: connection_graph) = 
  let id, name, cb = begin match c with
 | Arithmetic (id, cfg) -> id, "arithmetic-combinator", [json_of_config (A cfg)]
 | Decider (id, cfg) ->  id, "decider-combinator", [json_of_config (D cfg)]
 | Constant (id, cfg) -> id, "constant-combinator", [json_of_config (C cfg)]
 | Pole id -> id, "small-electric-pole", []
  end in 
  let conns = [] in 
  `Assoc ([("entity_number", `Int id); 
          ("name", `String name);
          ("position", `Assoc [("x", id * 2); ("y", 0)]);
          ] @ cb @ conns)

 let json_of_circuit (id: id) (circuit: circuit) = 
  let wire, combs, g = circuit in 
  List.map json_of_combinator combs 

let json_of_circuits (circuits: circuit list) = 
  let red, green = List.partition (fun (w, _, _) -> begin match w with
                                  | Red _ -> true | _ -> false end) circuits in
  let rm, gm = List.mapi json_of_circuit red, List.mapi json_of_circuit green in
  rm @ gm