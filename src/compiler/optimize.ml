open Ast.Bexp
open Ast.Circuit
open Ast.Combinator
open Layout
open Config
open FirstPhase 

let filter_connection_graph (g:connection_graph) (color:wire_color) : connection_graph = 
  let internal e acc =
    let _, c, _ = e in 
    if c = color then e :: acc else acc 
  in 

  let edges = CG.fold_edges_e internal g [] in
  let g = CG.create() in 
  List.iter (CG.add_edge_e g) edges; 
  g

let rec calculate_signals_on_wire (c:circuit) (color:wire_color) (conn:connection) : symbol list = 
  let combs, g, meta = c in 
  let (~!) = lookup combs in 
  let err = "Inconsistent connection graph" in 
  let filtered_g = filter_connection_graph g color in 
  let cs = calculate_signals_on_wire (combs, filtered_g, meta) in

  let map_aop conn (aop:aop) = 
    begin match aop with 
    | Const _ -> []
    | Symbol s -> [s]
    | Each -> let o = opposite_conn conn in 
              cs Red o @ cs Green o
  end in 

  let map_dop conn (dop:dop) = 
    begin match dop with 
    | Const _ -> []
    | Symbol s -> [s]
    | Anything
    | Everything
    | Each -> let o = opposite_conn conn in 
              cs Red o @ cs Green o
  end in 

  let intern conn acc = 
    begin match conn with 
    | Ain id -> 
      let comb =  ~!id in 
        begin match comb with 
        | Arithmetic (_, (o1, _, o2, _)) -> acc @ map_aop conn o1 @ map_aop conn o2
        | _ -> failwith err 
        end  
    | Aout id -> 
        let comb =  ~!id in 
        begin match comb with 
        | Arithmetic (_, (_, _, _, o)) -> acc @ map_aop conn o
        | _ -> failwith err 
        end  
    | Din id -> 
        let comb =  ~!id in 
        begin match comb with 
        | Decider (_, (o1, _, o2, _, _)) -> acc @ map_dop conn o1 @ map_dop conn o2
        | _ -> failwith err 
        end  
    | L id 
    | Dout id -> 
        let comb =  ~!id in 
        begin match comb with 
        | Decider (_, (_, _, _, o, _)) -> acc @ map_dop conn o
        | Lamp (_, (_, _, o)) -> acc @ map_dop conn o
        | _ -> failwith err 
        end  
    | C id -> 
        let comb =  ~!id in 
        begin match comb with 
        | Constant (_, sigs) -> acc @ List.map fst sigs
        | _ -> failwith err 
        end  
    | P id -> acc
    end
  in 
  let s = CG_traverse.fold_component intern [] g conn in 
  Core_kernel.List.stable_dedup s 

let rec input_signal_isolation (ctr: unit -> int) (inp_id:id) (circuit: circuit) : circuit = 
  let combs, g, meta = circuit in  
  let mid, i_sigs, o_sigs, iids, oids = meta in
  let internal acc id = 
    let conn = i_conn_of_id combs id in 
    let _ = get_primary () in (* ASSUMES ALL INPUT WIRES ARE PRIMARY COLOR *)
    (* let sigs = calculate_signals_on_wire circuit color conn in  *)
    let wires = CG.succ_e g conn in
    List.iter (fun (c1, color, c2) -> 

      (* if color <> input_color && c1 <> (P inp_id) && c2 <> (P inp_id) then  *)
      print_endline (string_of_edge (c1, color, c2));
      ) wires; 
    acc
  in
  print_endline("---");
  let _ = List.fold_left internal mid iids in 
  (combs, g, (mid, i_sigs, o_sigs, iids, oids))


let wrap_io (circ:concrete_circuit) : concrete_circuit = 
  let circuit, (origin, size, placements) = circ in 
  let combs, g, meta = circuit in
  let m_id, i_sigs, o_sigs, input, output = meta in 

  (* let get_entity_id = create_ctr ~i:(m_id + 1) () in  *)

  let has_input = List.length i_sigs > 0 in 

  let inp_id = get_entity_id () in
  let i_pole = Pole (inp_id, Substation) in 

  let ox, oy = origin in
  let sx, sy = size in 
  let y_level = oy +. float_of_int (sy / 2) in 

  let i_pole_placement = (ox -. 1., y_level) in 

  let wire_input conn = connect_primary g (P inp_id) conn in
  let wire_in i = wire_input (i_conn_of_id combs i) in

 
  let inp, inp_placements = if has_input then 
            let id = get_entity_id () in
            (wire_input (C id);
            [i_pole; Constant (id, List.map (fun v -> v, 1l) (List.sort Stdlib.compare i_sigs))], [i_pole_placement; (ox -. 1., y_level +. 1.5)]) 
            else [i_pole], [i_pole_placement] in

  let oid = get_entity_id () in 
  let o_pole = Pole (oid, Substation) in
  let o_placement = (ox +. float_of_int (sx + 1), y_level) in  

  let wire_out i = connect_primary g (o_conn_of_id combs i) (P oid) in

  (* List.iter wire_in input; *)
  (* List.iter (fun i -> print_endline (string_of_int i)) input; *)
  let rec connect_inputs input = 
    begin match input with 
    | [] -> () 
    | id :: [] -> wire_in id
    | id1 :: id2 :: tl  -> 
                           connect_primary g (i_conn_of_id combs id1) (i_conn_of_id combs id2);
                           connect_inputs (id2 :: tl)
    end in  
  let rec connect_outputs output = 
    begin match output with 
    | [] -> () 
    | id :: [] -> wire_out id
    | id1 :: id2 :: tl  -> 
                           connect_primary g (o_conn_of_id combs id1) (o_conn_of_id combs id2);
                           connect_outputs (id2 :: tl)
    end in  
  connect_inputs input;
  connect_outputs output;
  (* List.iter wire_out output; *)

  let new_combs = o_pole :: inp @ combs in
  let iids = List.map id_of_combinator inp in 

  let new_c = (new_combs, g, (oid, i_sigs, o_sigs, iids, [oid])) in
  let new_placements = o_placement :: inp_placements @ placements in 
  new_c, ((ox -. 2., Float.min y_level oy), (sx + 4, sy), new_placements)

  (* let combs, g, (mid, i_sigs, o_sigs, _, oids) = input_signal_isolation ctr inp_id new_c in
  (combs, g, (mid, i_sigs, o_sigs, iids, oids)) *)


(* basic optimization that optimizes from the structure of the bexp compiler
  essentially just removes redundant constant combinators. Doesn't update meta 
*)
  let rec primitive_optimization (circuit:circuit) : circuit = 
  let combs, g, meta = circuit in 
  let _, i_sigs, _, _, _ = meta in

  let (~$) c = id_of_conn c in
  let (~$$) c = lookup combs ~$c in

  let bind_conn_input cc oc i acc = 
    begin match lookup combs i with 
      | Constant (_, cfg) -> begin match cfg with 
                              | (s, v) :: [] -> if List.mem s i_sigs then acc else 
                                let comb = ~$$oc in 
                                (* let output_elim = 
                                  begin match comb with
                                  | Decider (_, (_, _, _, s, InpCount)) -> v = 1l && (match oc with Din _ -> true |_ -> false)   
                                  | _ -> false 
                                end in  *)
                                let output_elim = false in 
                                if (uses_signal_in_input comb s && not (uses_wildcard comb)) || output_elim then   
                                let d, n, sigs, dels = acc in 
                                let e = CG.succ_e g cc in            
                                d @ e, n, sigs @ List.map (fun _ -> comb, (s, v)) e, ~$$cc :: dels  
                                else acc 
                              | _ -> acc
                            end
      | _ -> failwith ("combinators inconsistent with connections - no CC with id " ^ string_of_int i)
      end in
  (* let bind_conn_output c1 c2 id1 id2 = 
    begin match lookup combs id1, lookup combs id2 with 
      | Constant (_, cfg), Decider (_) -> 
                              begin match cfg with 
                              | (s, v) :: [] -> if List.mem s i_sigs then c1, c2 else 
                                let comb = ~$$c2 in 
                                if (uses_signal_in_input comb s && not (uses_wildcard comb)) then   
                                let d, n, sigs, dels = acc in 
                                let e = CG.succ_e g cc in            
                                d @ e, n, sigs @ List.map (fun _ -> comb, (s, v)) e, ~$$cc :: dels  
                                else acc 
                              | _ -> c1, c2
                            end
      | _ -> failwith ("combinators inconsistent with connections - no CC/DC with id " ^ string_of_int i)
      end in *)

  let delete_edges, new_edges, sigs, deleted_combs = CG.fold_edges (fun c1 c2 acc -> 
    (* outputs *)
    let c1, c2 = begin match c1, c2 with 
    | C id1, Din id2
    | Din id2, C id1 -> c1, c2
    | _ -> c1, c2 
    end in 

    (* inputs *)
    begin match c1, c2 with 
    | C _, P _  
    | P _, C _ -> acc
    | C i, _ -> bind_conn_input c1 c2 i acc 
    | _, C i -> bind_conn_input c2 c1 i acc 
    | _ -> acc
    end
    ) g ([], [], [], []) in

  let a_combs = List.map (fun (c, s) -> c) sigs in
  let temp = a_combs @ deleted_combs in
  let u_combs = List.filter (fun c -> not (List.mem c temp)) combs in 

  let new_combs = List.map (fun (c, (s,v)) -> begin match c with 
                                          | Arithmetic c -> Arithmetic (replace_signal_A c s v)
                                          | Decider c -> Decider (replace_signal_D c s v)
                                          | _ -> failwith "impossible"
                                          end)
                                          sigs in 
                     
  List.iter (CG.remove_edge_e g) delete_edges;
  List.iter (CG.add_edge_e g) new_edges;
  (u_combs @ new_combs, g, meta)

(* Remap ids after combinators have been deleted through optimization passes *)
let remap_ids ?reset_ctr:(reset_ctr=true) (circuits:circuit list) : circuit list = 
  if reset_ctr then reset_entity_ctr ();

  let remap circuit = 
    let combs, g, meta = circuit in 
    let _, i_sigs, o_sigs, input, output = meta in
    let id_map = List.map (fun c -> let i = get_entity_id () in id_of_combinator c, i) combs in 

    let (~!) i = List.assoc i id_map in 
    let (~$) c = ~!(id_of_conn c) in
    let (~$$) c = ~!(id_of_combinator c) in

    let new_g = CG.create () in 

    let remap_conn c =
      let id = ~$c in
      begin match c with 
      | Ain i -> Ain id
      | Aout i -> Aout id
      | Din i -> Din id
      | Dout i -> Dout id
      | C i -> C id
      | L i -> L id
      | P i -> P id
      end in

    CG.iter_edges_e (fun (c1, c, c2) -> connect c new_g (remap_conn c1) (remap_conn c2)) g;
    let new_combs = List.map (fun c -> let i = ~$$c in begin match c with 
                                          | Arithmetic (_, cfg) -> Arithmetic (i, cfg)
                                          | Decider (_, cfg) -> Decider (i, cfg)
                                          | Constant (_, cfg) -> Constant (i, cfg)
                                          | Lamp (_, cfg) -> Lamp (i, cfg)
                                          | Pole (_, t) -> Pole (i, t)
                                          end) combs in 

    let m_id = List.fold_left (fun acc (_, i) -> max acc i) (-1) id_map in

    let new_input = List.map (~!) input in 
    let new_output = List.map (~!) output in 

    (new_combs, new_g, (m_id, i_sigs, o_sigs, new_input, new_output))
  in
  List.map remap circuits 

let remap_ids_concrete ?reset_ctr:(reset_ctr=true) (circuits:concrete_circuit list) : concrete_circuit list = 
  let raw, layouts = List.split circuits in 
  let remapped = remap_ids ~reset_ctr raw in 
  List.combine remapped layouts
