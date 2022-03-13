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

module Node = struct                                                                
   type t = connection                                                                     
   let compare = Stdlib.compare                                                 
   let hash = Hashtbl.hash                                                          
   let equal = (=)                                                                  
end                                                                                 

module CG = Graph.Imperative.Graph.Concrete(Node)
module CG_ops = Graph.Oper.I(CG)

type connection_graph = CG.t

(* max_id, input sigs, output sigs, input ids, output ids*)
type circuit_meta = id * (symbol list) * (symbol list) * (id list) * (id list) 

type circuit = wire * combinator list * connection_graph * circuit_meta

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

let succs g id = 
  if CG.mem_vertex g id then CG.succ g id else []