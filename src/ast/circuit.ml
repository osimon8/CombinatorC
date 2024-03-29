open Combinator
open Utils

type wire_color = Red | Green
type connection = Ain of id | Aout of id | Din of id | Dout of id | C of id | L of id | P of id

module Node = struct
  type t = connection

  let compare = Stdlib.compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module Edge = struct
  type t = wire_color

  let compare = Stdlib.compare
  let equal = ( = )
  let default = Red
end

module CG = Graph.Imperative.Graph.ConcreteLabeled (Node) (Edge)
module CG_ops = Graph.Oper.I (CG)
module CG_traverse = Graph.Traverse.Bfs (CG)

type connection_graph = CG.t

(* max_id, input sigs, output sigs, input ids, output ids*)
type circuit_meta = {
  max_id : id;
  input_sigs : symbol list;
  output_sigs : symbol list;
  input_ids : id list;
  output_ids : id list;
}

type circuit = combinator list * connection_graph * circuit_meta
type composition = circuit -> circuit -> circuit
type placement = float * float
type loc = placement option

let origin = ref (0., 0.)
let get_origin () = !origin
let set_origin new_o = origin := new_o

type circuit_layout = placement * size * placement list
type concrete_circuit = circuit * circuit_layout

let entity_ctr = ref (create_ctr ())

let get_entity_id () : id =
  let v = !entity_ctr () in
  (* print_endline ("INCED ENTITY CTR, RETURNING: " ^ string_of_int v); *)
  v

let reset_entity_ctr () = entity_ctr := create_ctr ()

let mid_of_circuit (circuit : circuit) : id =
  let _, _, { max_id } = circuit in
  max_id

(* input pole id, output pole id, circuit *)
(* type wrapped_circuit = id * id * circuit  *)
let id_of_conn conn : id =
  begin
    match conn with
    | Ain id | Aout id | Din id | Dout id | C id | L id | P id -> id
  end

let opposite_conn conn : connection =
  begin
    match conn with
    | Ain id -> Aout id
    | Aout id -> Ain id
    | Din id -> Dout id
    | Dout id -> Din id
    | C _ | L _ | P _ -> conn
  end

(* id used by Factorio in circuit id in JSON representation *)
let type_id_of_conn conn =
  begin
    match conn with
    | Ain _ | Din _ | C _ | L _ | P _ -> 1
    | Aout _ | Dout _ -> 2
  end

let string_of_conn conn =
  begin
    match conn with
    | Ain id -> "(Ain " ^ string_of_int id ^ ")"
    | Aout id -> "(Aout " ^ string_of_int id ^ ")"
    | Din id -> "(Din " ^ string_of_int id ^ ")"
    | Dout id -> "(Dout " ^ string_of_int id ^ ")"
    | C id -> "(C " ^ string_of_int id ^ ")"
    | L id -> "(L " ^ string_of_int id ^ ")"
    | P id -> "(P " ^ string_of_int id ^ ")"
  end

let string_of_wire_color (wc : wire_color) =
  begin
    match wc with
    | Red -> "red"
    | Green -> "green"
  end

let string_of_edge e =
  let v1, c, v2 = e in
  string_of_conn v1 ^ " <-> " ^ string_of_conn v2 ^ " : " ^ string_of_wire_color c

let print_edges (g : connection_graph) : unit =
  print_endline "PRINTING GRAPH";
  CG.iter_edges_e (fun e -> print_endline (string_of_edge e)) g;
  print_endline "--------------------"

let succs g id = if CG.mem_vertex g id then CG.succ_e g id else []
