open Ast
open Circuit
open Combinator
open To_json
open Utils

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

let compile_bexp_to_circuit ?output_sig:(output_sig="check") (b: bexp) : circuit list = 
  let vars = vars_in_bexp b in 
  let sigs = output_sig :: vars in

  let entity_ctr = create_ctr () in
  let sig_ctr = create_sig_ctr sigs in

  let g = CG.create () in 

  (* let wire_input conn = CG.add_edge g (P inp_id) conn in
  let wire_A i = wire_input (Ain i) in
  let wire_D i = wire_input (Din i) in *)
  let connect c1 c2 = 
    if id_of_conn c1 <> -1 && id_of_conn c2 <> -1 then CG.add_edge g c1 c2 in  

  let rec cb b = 
    let a_binop b1 b2 aop = 
      let o_sig = sig_ctr () in
      let id = entity_ctr () in
      let c, i = 
        begin match b1, b2 with 
          | Lit l1, Lit l2 -> [ Arithmetic (id, (Const l1, aop, Const l2, Symbol o_sig)) ], [] 
          | Lit l, Var v -> [ Arithmetic (id, (Const l, aop, Symbol v, Symbol o_sig)) ], [id] 
          | Var v, Lit l -> [ Arithmetic (id, (Symbol v, aop, Const l, Symbol o_sig)) ], [id] 
          | Var v1, Var v2 ->[ Arithmetic (id, (Symbol v1, aop, Symbol v2, Symbol o_sig)) ], [id] 
          | _ -> let c1, iids1, o1, s1 = cb b1 in 
                  let c2, iids2, o2, s2 = cb b2 in 
                connect o1 (Ain id);
                connect o2 (Ain id);
                c1 @ c2 @ [ Arithmetic (id, (Symbol s1, aop, Symbol s2, Symbol o_sig)) ], iids1 @ iids2
        end in 
    c, i, Aout id, o_sig in

    let d_binop b1 b2 dop = 
      let o_sig = sig_ctr () in
      let id = entity_ctr () in
      let c, i = 
        begin match b1, b2 with 
          | Lit l1, Lit l2 -> [ Decider (id, (Const l1, dop, Const l2, Symbol o_sig, One))], []
          | Lit l, Var v -> [ Decider (id, (Const l, dop, Symbol v, Symbol o_sig, One))], [id]
          | Var v, Lit l -> [ Decider (id, (Symbol v, dop, Const l, Symbol o_sig, One))], [id]
          | Var v1, Var v2 -> [ Decider (id, (Symbol v1, dop, Symbol v2, Symbol o_sig, One))], [id] 
          | _ -> let c1, iids1, o1, s1 = cb b1 in 
                  let c2, iids2, o2, s2 = cb b2 in 
                connect o1 (Din id);
                connect o2 (Din id);
                c1 @ c2 @ [ Decider (id, (Symbol s1, dop, Symbol s2, Symbol o_sig, One)) ], iids1 @ iids2
          end
      in c, i, Dout id, o_sig  in

    let logi b1 b2 aop dop = 
      let c1, iids1, o1, s1 = cb b1 in 
      let c2, iids2, o2, s2 = cb b2 in
      let id1 = entity_ctr () in 
      let id2 = entity_ctr () in 
      let t_sig = sig_ctr () in 
      let o_sig = sig_ctr () in
      connect o1 (Ain id1);
      connect o2 (Ain id1);
      connect (Aout id1) (Din id2);
      c1 @ c2 @ [ Arithmetic (id1, (Symbol s1, aop, Symbol s2, Symbol t_sig) );
                  Decider (id2, (Symbol t_sig, dop, Const 0, Symbol o_sig, One)) ]
      , iids1 @ iids2, Dout id2, o_sig
    in

    let bool_cast b dop= 
      let c, iids, o, s = cb b in 
      let id = entity_ctr () in 
      let o_sig = sig_ctr () in
      connect o (Din id);
      c @ [ Decider (id, (Symbol s, dop, Const 0, Symbol o_sig, One)) ], iids, Dout id, o_sig
    in

    begin match b with 
    (* | Var v -> [], P inp_id, v we don't need new combinators, set output to the input pole  *)
    | Var v -> [], [], P (-1), v (* IO wrapping will handle, set to dummy conn  *)
    | Lit l -> let s = sig_ctr () in 
              let id = entity_ctr () in 
              [ Constant (id, [(s, l)]) ], [], C id, s (* lone lit is a constant *)
    | Neg b -> let c, iids, o, s = cb b in 
              let id = entity_ctr () in 
              let o_sig = sig_ctr () in
              connect o (Ain id);
              c @ [ Arithmetic (id, (Symbol s, Mul, Const (-1), Symbol o_sig)) ], iids, Aout id, o_sig
    | Not b -> bool_cast b Eq
    | BOOL b -> bool_cast b Neq
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
  
  let combs, iids, o_conn, o_sig = cb b in
  let oid = id_of_conn o_conn in 
  let fix_o_sig c = 
    let id = id_of_combinator c in 
    if id = oid then
        begin match c with
        | Arithmetic (_, (op1, aop, op2, _)) -> Arithmetic (id, (op1, aop, op2, Symbol output_sig))
        | Decider (_, (op1, dop, op2, _, t)) -> Decider (id, (op1, dop, op2, Symbol output_sig, t))
        | Constant (_, sigs) -> Constant (id, List.map (fun (_, v) -> output_sig, v) sigs) 
        | Pole _ -> c
      end
  else c 
  in 

  let combs = List.map fix_o_sig combs in 

  let m_id = List.fold_left (fun acc c -> max acc (id_of_combinator c)) (-1) combs in

  [ (Red [], combs, g, (m_id, vars, [o_sig], iids, [oid])) ]

let lookup (id:id) (combs:combinator list) : combinator =
  List.find (fun c -> id_of_combinator c = id) combs 

let i_conn_of_id (id:id) (combs: combinator list) : connection =
  begin match lookup id combs with 
  | Arithmetic _ -> Ain id 
  | Decider _ -> Din id 
  | Constant _ -> C id 
  | Pole _ -> P id 
  end

let o_conn_of_id (id:id) (combs: combinator list) : connection =
  begin match lookup id combs with 
  | Arithmetic _ -> Aout id 
  | Decider _ -> Dout id 
  | Constant _ -> C id 
  | Pole _ -> P id 
  end

let wrap_io (ctr: unit -> value) (circuit:circuit) : circuit = 
  let wire, combs, g, meta = circuit in
  let m_id, i_sigs, o_sigs, input, output = meta in 

  let entity_ctr = create_ctr ~i:(m_id + 1) () in 
  let has_input = List.length i_sigs > 0 in 

  let inp_id = entity_ctr () in
  let i_pole = Pole (inp_id) in 

  let wire_input conn = CG.add_edge g (P inp_id) conn in
  let wire_in i = wire_input (i_conn_of_id i combs) in

  let inp = if has_input then 
            let id = entity_ctr () in
            (wire_input (C id);
            [i_pole; Constant (id, List.map (fun v -> v, 1) i_sigs)]) 
            else [i_pole] in

  let oid = entity_ctr () in 
  let o_pole = Pole (oid) in

  let wire_out i = CG.add_edge g (o_conn_of_id i combs) (P oid) in

  List.iter wire_in input;
  List.iter wire_out output;

  let new_combs = o_pole :: inp @ combs in
  let iids = List.map id_of_combinator inp in 

  (wire, new_combs, g, (entity_ctr () - 1, i_sigs, o_sigs, iids, [oid]))

(* basic optimization that optimizes from the structure of the bexp compiler
  essentially just removes redundant constant combinators. Doesn't update meta 
*)
let rec primitive_optimization (circuit:circuit) : circuit = 
  let wire, combs, g, meta = circuit in 
  let _, i_sigs, _, _, _ = meta in

  let (~$) c = id_of_conn c in
  let (~$$) c = lookup ~$c combs in

  let bind_conn cc oc i acc = 
    begin match lookup i combs with 
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
  (wire, u_combs @ new_combs, g, meta)

(* Remap ids after combinators have been deleted through optimization passes *)
let remap_ids (ctr: unit -> value) (circuit:circuit) : circuit = 
  let wire, combs, g, meta = circuit in 
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

  CG.iter_edges (fun c1 c2 -> CG.add_edge new_g (remap_conn c1) (remap_conn c2)) g;
  let new_combs = List.map (fun c -> let i = ~$$c in begin match c with 
                                        | Arithmetic (_, cfg) -> Arithmetic (i, cfg)
                                        | Decider (_, cfg) -> Decider (i, cfg)
                                        | Constant (_, cfg) -> Constant (i, cfg)
                                        | Pole _ -> Pole i
                                        end) combs in 

  let m_id = List.fold_left (fun acc (_, i) -> max acc i) (-1) id_map in
  let new_input = List.map (~!) input in 
  let new_output = List.map (~!) output in 

  (wire, new_combs, new_g, (m_id, i_sigs, o_sigs, new_input, new_output))

let circuit_concat (c1:circuit) (c2:circuit) : circuit = 
  let wire, combs1, g1, meta = c1 in 
  let m1, i_sigs2, o_sigs1, input1, output1 = meta in

  let _, combs2, g2, meta = c1 in 
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

  (wire, combs1 @ combs2, new_g, (m, i_sigs, o_sigs, input, output))

let compile_bexp_to_json ?optimize:(optimize=true) (b: bexp) : json list = 
  let circuits = compile_bexp_to_circuit b in 
  let circuits = 
    if optimize then 
      let ctr = create_ctr () in 
      (* List.iter (fun (_, _, g, _) -> print_edges g) circuits; *)
      let circuits_opt = List.map primitive_optimization circuits in
      let circuits_opt = List.map (remap_ids ctr) circuits_opt in
      List.map (wrap_io ctr) circuits_opt 
    else circuits in
  let json_list = json_of_circuits circuits in 
  json_list
