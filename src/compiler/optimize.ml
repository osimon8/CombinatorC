open Ast
open Circuit
open Combinator
open Utils
open Config

let sig_list = 
  ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; 
   "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; 
   "K"; "L"; "M"; "N"; "O"; "P"; "Q"; "R"; "S"; "T"; 
   "U"; "V"; "W"; "X"; "Y"; "Z"; "red"; "green"; "blue"; 
   "yellow"; "magenta"; "cyan"; "white"; "gray"; "black"; 
   "info"; "dot"; "check"]

let conns_of_comb (c: combinator) : connection list = 
  begin match c with 
    | Pole i -> [ P i ]
    | Constant (i, _) -> [ C i ]
    | Arithmetic (i, _) -> [ Ain i; Aout i] 
    | Decider (i, _) -> [ Din i; Dout i ]
  end

let conns_of_combs (combs: combinator list) : connection list = 
  List.concat_map conns_of_comb combs

let create_sig_ctr (vars:string list) = 
  let ctr = create_ctr ~i:0 () in 
  let rec intern v = 
    begin match List.nth_opt sig_list v with 
    | None -> failwith "Out of signals!" 
    | Some s -> if List.mem s vars then intern (ctr ()) else s
    end
  in
  fun () -> 
    let v = ctr () in
    intern v

let connect c g c1 c2 = 
  CG.add_edge_e g (c1, c, c2)

let connect_primary g c1 c2 = 
  connect (get_primary ()) g c1 c2 

let connect_secondary g c1 c2 = 
  connect (get_secondary ()) g c1 c2 

let connect_rand g c1 c2 = 
  let f = if Random.bool() then connect_primary else connect_secondary in 
  f g c1 c2

let circuit_of_bexp i (output_sig:symbol) (b: bexp) : circuit = 
  let vars = vars_in_bexp b in 
  let sigs = output_sig :: vars in

  let entity_ctr = create_ctr ~i () in
  let sig_ctr = create_sig_ctr sigs in

  let g = CG.create () in 

  let connect c1 c2 = 
    List.iter (fun c1 -> List.iter (connect_primary g c1) c2) c1
     in
  
  let connect2 c1 c2 = 
    List.iter (fun c1 -> List.iter (connect_secondary g c1) c2) c1
     in

  let get_o_sig b = 
    begin match b with 
    | Var v -> v
    | _ -> sig_ctr () 
  end in 

  let rec cb ?output_sig:(output_sig=None) b : combinator list * (id list) option * connection list * symbol = 
    let o_sig = 
      begin match output_sig with
      | None -> get_o_sig b 
      | Some s -> s
    end in  

    let bin_iid_map id iids1 iids2 = 
      begin match iids1, iids2 with 
        | None, None -> [id] 
        | None, Some i1  
        | Some i1, None -> id :: i1
        | Some i1, Some i2 -> i1 @ i2  
      end in 

    let unary_iid_map id iids = 
      begin match iids with 
        | None -> [id] 
        | Some l -> id :: l 
      end in 

    let a_binop b1 b2 aop = 
      let id = entity_ctr () in
      let c, i = 
        begin match b1, b2 with 
          | Lit l1, Lit l2 -> [ Arithmetic (id, (Const l1, aop, Const l2, Symbol o_sig)) ], [] 
          | Lit l, Var v -> [ Arithmetic (id, (Const l, aop, Symbol v, Symbol o_sig)) ], [id] 
          | Var v, Lit l -> [ Arithmetic (id, (Symbol v, aop, Const l, Symbol o_sig)) ], [id] 
          | Var v1, Var v2 ->[ Arithmetic (id, (Symbol v1, aop, Symbol v2, Symbol o_sig)) ], [id] 
          | _ -> let c1, iids1, o1, s1 = cb b1 in 
                  let c2, iids2, o2, s2 = cb b2 in 
                  let iids = bin_iid_map id iids1 iids2 in 
                connect (o1 @ o2) [Ain id];
                c1 @ c2 @ [ Arithmetic (id, (Symbol s1, aop, Symbol s2, Symbol o_sig)) ], iids
        end in 
    c, Some i, [Aout id], o_sig in

    let d_binop b1 b2 dop = 
      let id = entity_ctr () in
      let c, i = 
        begin match b1, b2 with 
          | Lit l1, Lit l2 -> [ Decider (id, (Const l1, dop, Const l2, Symbol o_sig, One))], []
          | Lit l, Var v -> [ Decider (id, (Const l, dop, Symbol v, Symbol o_sig, One))], [id]
          | Var v, Lit l -> [ Decider (id, (Symbol v, dop, Const l, Symbol o_sig, One))], [id]
          | Var v1, Var v2 -> [ Decider (id, (Symbol v1, dop, Symbol v2, Symbol o_sig, One))], [id] 
          | _ -> let c1, iids1, o1, s1 = cb b1 in 
                 let c2, iids2, o2, s2 = cb b2 in 
                 let iids = bin_iid_map id iids1 iids2 in 
                connect (o1 @ o2) [Din id];
                c1 @ c2 @ [ Decider (id, (Symbol s1, dop, Symbol s2, Symbol o_sig, One)) ], iids
          end
      in c, Some i, [Dout id], o_sig  in

    let logi b1 b2 aop dop = 
      let c1, iids1, o1, s1 = cb b1 in 
      let c2, iids2, o2, s2 = cb b2 in
      let id1 = entity_ctr () in 
      let id2 = entity_ctr () in 
      let t_sig = sig_ctr () in 
      let iids = bin_iid_map id1 iids1 iids2 in 

      connect (o1 @ o2) [Ain id1];
      connect [Aout id1] [Din id2];
      c1 @ c2 @ [ Arithmetic (id1, (Symbol s1, aop, Symbol s2, Symbol t_sig) );
                  Decider (id2, (Symbol t_sig, dop, Const 0, Symbol o_sig, One)) ]
      , Some iids, [Dout id2], o_sig
    in

    let isolate b dop dtype = 
      let c, iids, o, s = cb b in 
      let id = entity_ctr () in 
      let iids = unary_iid_map id iids in 
      connect o [Din id];
      c @ [ Decider (id, (Symbol s, dop, Const 0, Symbol o_sig, dtype)) ], Some iids, [Dout id], o_sig
    in      

    let conditional b1 b2 b3 = 
      let c1, iids1, o1, guard = cb b1 in 
      let c2, iids2, o2, _ = cb ~output_sig:(Some o_sig) b2 in 
      let c3, iids3, o3, _ = cb ~output_sig:(Some o_sig) b3 in 
      let id1 = entity_ctr () in 
      let id2 = entity_ctr () in 
      connect o1 [Din id1; Din id2];
      connect2 o2 [Din id1];
      connect2 o3 [Din id2];
      let niids1 = bin_iid_map id1 iids1 iids2 in 
      let niids2 = bin_iid_map id2 iids1 iids3 in 

      let combs = [Decider (id1, (Symbol guard, Neq, Const 0, Symbol o_sig, InpCount)); 
                   Decider (id2, (Symbol guard, Eq, Const 0, Symbol o_sig, InpCount))] in 
      c1 @ c2 @ c3 @ combs, Some (niids1 @ niids2), [Dout id1; Dout id2], o_sig
    in

    begin match b with 
    | Var v -> 
      if v <> o_sig then 
        let id = entity_ctr () in 
        let combs = [Arithmetic (id, (Symbol v, Add, Const 0, Symbol o_sig))] in 
        combs, Some [id], [Aout id], o_sig
      else 
      [], None, [], o_sig (* IO wrapping will handle, do nothing.
                           Setting iids to None signals that the upper level needs to be an input   *)
    | Lit l -> 
              let id = entity_ctr () in 
              [ Constant (id, [(o_sig, l)]) ], Some [], [C id], o_sig (* lone lit is a constant *)
    | Neg b -> 
              let c, iids, o, s = cb b in 
              let id = entity_ctr () in 
              let iids = unary_iid_map id iids in 
              connect o [Ain id];
              c @ [ Arithmetic (id, (Symbol s, Mul, Const (-1), Symbol o_sig)) ], Some iids, [Aout id], o_sig
    | Conditional (b1, b2, b3) -> conditional b1 b2 b3
    | Not b -> isolate b Eq One
    | BOOL b -> isolate b Neq One
    | Plus (b1, b2) -> a_binop b1 b2 Add
    | Minus (b1, b2) -> a_binop b1 b2 Sub
    | Mul (b1, b2) -> a_binop b1 b2 Mul
    | Div (b1, b2) -> a_binop b1 b2 Div
    | Mod (b1, b2) -> a_binop b1 b2 Mod
    | Exp (b1, b2) -> a_binop b1 b2 Exp
    | Lshift (b1, b2) -> a_binop b1 b2 Lshift
    | Rshift (b1, b2) -> a_binop b1 b2 Rshift
    | AND (b1, b2) -> a_binop b1 b2 AND
    | OR (b1, b2) -> a_binop b1 b2 OR
    | XOR (b1, b2) -> a_binop b1 b2 XOR
    | Gt (b1, b2) -> d_binop b1 b2 Gt
    | Lt (b1, b2) -> d_binop b1 b2 Lt
    | Gte (b1, b2) -> d_binop b1 b2 Gte
    | Lte (b1, b2) -> d_binop b1 b2 Lte
    | Eq (b1, b2) -> d_binop b1 b2 Eq
    | Neq (b1, b2) -> d_binop b1 b2 Neq
    | LAND (b1, b2) -> logi b1 b2 Mul Neq
    | LOR (b1, b2) -> logi b1 b2 OR Neq
    | NAND (b1, b2) -> logi b1 b2 Mul Eq
    | NOR (b1, b2) -> logi b1 b2 OR Eq
    end 
  in 
  
  let combs, iids, o_conns, o_sig = cb ~output_sig:(Some(output_sig)) b in
  let iids = begin match iids with 
  | Some l -> l 
  | None -> [] 
  end in 
  let oids = List.map id_of_conn o_conns in 

  let m_id = List.fold_left (fun acc c -> max acc (id_of_combinator c)) Int.min_int combs in
  (* List.iter (fun i -> print_endline (string_of_int i)) iids;
  print_endline "+++"; *)

 (combs, g, (m_id, vars, [o_sig], iids, oids))

let lookup (combs:combinator list) (id:id)  : combinator =
  List.find (fun c -> id_of_combinator c = id) combs 

let i_conn_of_id (id:id) (combs: combinator list) : connection =
  begin match lookup combs id with 
  | Arithmetic _ -> Ain id 
  | Decider _ -> Din id 
  | Constant _ -> C id 
  | Pole _ -> P id 
  end

let o_conn_of_id (id:id) (combs: combinator list) : connection =
  begin match lookup combs id with 
  | Arithmetic _ -> Aout id 
  | Decider _ -> Dout id 
  | Constant _ -> C id 
  | Pole _ -> P id 
  end

(* basic optimization that optimizes from the structure of the bexp compiler
  essentially just removes redundant constant combinators. Doesn't update meta 
*)

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
    | Dout id -> 
        let comb =  ~!id in 
        begin match comb with 
        | Decider (_, (_, _, _, o, _)) -> acc @ map_dop conn o
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

let rec input_signal_isolation (ctr: unit -> value) (inp_id:id) (circuit: circuit) : circuit = 
  let combs, g, meta = circuit in  
  let mid, i_sigs, o_sigs, iids, oids = meta in
  let internal acc id = 
    let conn = i_conn_of_id id combs in 
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


let wrap_io (ctr: unit -> value) (circuit:circuit) : circuit = 
  let combs, g, meta = circuit in
  let m_id, i_sigs, o_sigs, input, output = meta in 

  let entity_ctr = create_ctr ~i:(m_id + 1) () in 
  let has_input = List.length i_sigs > 0 in 

  let inp_id = entity_ctr () in
  let i_pole = Pole (inp_id) in 

  let wire_input conn = connect_primary g (P inp_id) conn in
  let wire_in i = wire_input (i_conn_of_id i combs) in

  let inp = if has_input then 
            let id = entity_ctr () in
            (wire_input (C id);
            [i_pole; Constant (id, List.map (fun v -> v, 1) i_sigs)]) 
            else [i_pole] in

  let oid = entity_ctr () in 
  let o_pole = Pole (oid) in

  let wire_out i = connect_primary g (o_conn_of_id i combs) (P oid) in

  List.iter wire_in input;
  List.iter wire_out output;

  let new_combs = o_pole :: inp @ combs in
  let iids = List.map id_of_combinator inp in 

  let new_c = (new_combs, g, (oid, i_sigs, o_sigs, iids, [oid])) in
  new_c

  (* let combs, g, (mid, i_sigs, o_sigs, _, oids) = input_signal_isolation ctr inp_id new_c in
  (combs, g, (mid, i_sigs, o_sigs, iids, oids)) *)

let rec primitive_optimization (circuit:circuit) : circuit = 
  let combs, g, meta = circuit in 
  let _, i_sigs, _, _, _ = meta in

  let (~$) c = id_of_conn c in
  let (~$$) c = lookup combs ~$c in

  let bind_conn cc oc i acc = 
    begin match lookup combs i with 
      | Constant (_, cfg) -> begin match cfg with 
                              | (s, v) :: [] -> if List.mem s i_sigs then acc else 
                              let d, n, sigs, dels = acc in 
                              let e = CG.succ_e g cc in            
                              d @ e, n, sigs @ List.map (fun _ -> ~$$oc, (s, v)) e, ~$$cc :: dels  
                              | _ -> acc
                            end
      | _ -> failwith ("combinators inconsistent with connections - no CC with id " ^ string_of_int i)
      end in

  let delete_edges, new_edges, sigs, deleted_combs = CG.fold_edges (fun c1 c2 acc -> 
    begin match c1, c2 with 
    | C _, P _  
    | P _, C _ -> acc
    | C i, _ -> bind_conn c1 c2 i acc 
    | _, C i -> bind_conn c2 c1 i acc 
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
let remap_ids (ctr: unit -> value) (circuit:circuit) : circuit = 
  let combs, g, meta = circuit in 
  let _, i_sigs, o_sigs, input, output = meta in
  let id_map = List.map (fun c -> let i = ctr() in id_of_combinator c, i) combs in 

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
    | P i -> P id
    end in

  CG.iter_edges_e (fun (c1, c, c2) -> connect c new_g (remap_conn c1) (remap_conn c2)) g;
  let new_combs = List.map (fun c -> let i = ~$$c in begin match c with 
                                        | Arithmetic (_, cfg) -> Arithmetic (i, cfg)
                                        | Decider (_, cfg) -> Decider (i, cfg)
                                        | Constant (_, cfg) -> Constant (i, cfg)
                                        | Pole _ -> Pole i
                                        end) combs in 

  let m_id = List.fold_left (fun acc (_, i) -> max acc i) (-1) id_map in

  let new_input = List.map (~!) input in 
  let new_output = List.map (~!) output in 

  (new_combs, new_g, (m_id, i_sigs, o_sigs, new_input, new_output))

let circuit_concat (c1:circuit) (c2:circuit) : circuit = 
  let combs1, g1, meta = c1 in 
  let m1, i_sigs2, o_sigs1, input1, output1 = meta in

  let combs2, g2, meta = c1 in 
  let m2, i_sigs1, o_sigs2, input2, output2 = meta in

  let new_g = CG_ops.union g1 g2 in 

  let l1 id = o_conn_of_id id combs1 in 
  let l2 id = i_conn_of_id id combs2 in 

  let inter oid = 
    List.iter (fun iid -> CG.add_edge new_g (l1 oid) (l2 iid)) input2
  in

  List.iter inter output1;

  let m = max m1 m2 in 
  let i_sigs = i_sigs1 in 
  let o_sigs = o_sigs2 in 
  let input = input1 in 
  let output = output2 in 

  (combs1 @ combs2, new_g, (m, i_sigs, o_sigs, input, output))


