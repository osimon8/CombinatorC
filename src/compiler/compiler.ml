open Ast
open Circuit
open Combinator
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

let compile_bexp_to_circuit (b: bexp) : circuit list = 
  let vars = vars_in_bexp b in 
  let has_vars = List.length vars > 0 in

  let entity_ctr = create_ctr () in
  let sig_ctr = create_sig_ctr vars in

  let inp_id = entity_ctr () in
  let i_pole = Pole (inp_id) in 
  let g = CG.create () in 

  let wire_input conn = CG.add_edge g (P inp_id) conn in
  let wire_A i = wire_input (Ain i) in

  let rec cb b = 
    let binop b1 b2 aop = 
      let o_sig = sig_ctr () in
      let id = entity_ctr () in
      (* let wire_input () = CG.add_edge g (P inp_id) (Ain id) in *)
      begin match b1, b2 with 
                            | Lit l1, Lit l2 -> [ Arithmetic (id, (Const l1, aop, Const l2, Symbol o_sig)) ]
                            | Lit l, Var v -> wire_A id; [ Arithmetic (id, (Const l, aop, Symbol v, Symbol o_sig)) ]
                            | Var v, Lit l -> wire_A id; [ Arithmetic (id, (Symbol v, aop, Const l, Symbol o_sig)) ]
                            | Var v1, Var v2 -> wire_A id; [ Arithmetic (id, (Symbol v1, aop, Symbol v2, Symbol o_sig)) ]
                            | _ -> let c1, o1, s1 = cb b1 in 
                                   let c2, o2, s2 = cb b2 in 
                                  CG.add_edge g o1 (Ain id);
                                  CG.add_edge g o2 (Ain id);
                                  c1 @ c2 @ [ Arithmetic (id, (Symbol s1, aop, Symbol s2, Symbol o_sig)) ]
                            end, Aout id, o_sig in

    begin match b with 
    | Var v -> [], P inp_id, v (* we don't need new combinators, the rec loop will wire to the input *)
    | Lit l -> let s = sig_ctr () in 
              let id = entity_ctr () in 
              [ Constant (id, [(s, l)]) ], C id, s (* lone lit is a constant *)
    | Neg b -> let c, o, s = cb b in 
              let id = entity_ctr () in 
              let o_sig = sig_ctr () in
              CG.add_edge g o (Ain id);
              c @ [ Arithmetic (id, (Symbol s, Mul, Const (-1), Symbol o_sig)) ], Aout id, o_sig
    | Plus (b1, b2) -> binop b1 b2 Add
    | Minus (b1, b2) -> binop b1 b2 Sub
    | Mul (b1, b2) -> binop b1 b2 Mul
    | Div (b1, b2) -> binop b1 b2 Div
    | Mod (b1, b2) -> binop b1 b2 Mod
    | Exp (b1, b2) -> binop b1 b2 Exp
    | Lshift (b1, b2) -> binop b1 b2 Lshift
    | Rshift (b1, b2) -> binop b1 b2 Rshift
    | AND (b1, b2) -> binop b1 b2 AND
    | OR (b1, b2) -> binop b1 b2 OR
    | XOR (b1, b2) -> binop b1 b2 XOR
    end 
  in 
  
  let inp = if has_vars then 
            let id = entity_ctr () in
            (wire_input (C id);
            [i_pole; Constant (id, List.map (fun v -> v, 1) vars)]) 
            else [i_pole] in

  let cmbs, o_conn, _ = cb b in

  let o_pole = Pole (entity_ctr ()) in

  let combs = inp @ cmbs @ [ o_pole ] in 

  CG.add_edge g o_conn (P (id_of_combinator o_pole));

  [ (Red [], combs, g) ]

let lookup (id:id) (combs:combinator list) : combinator =
  List.find (fun c -> id_of_combinator c = id) combs 

(* basic optimization that optimizes from the structure of the bexp compiler
  essentially just removes redundant constant combinators
*)
let rec primitive_optimization (vars:string list) (circuit:circuit) : circuit = 
  let wire, combs, g = circuit in 

  let (~$) c = id_of_conn c in
  let (~$$) c = lookup ~$c combs in

  let bind_conn cc oc i acc = 
    begin match lookup i combs with 
      | Constant (_, cfg) -> begin match cfg with 
                              | (s, v) :: [] -> if List.mem s vars then acc else 
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
                                          | Arithmetic c -> Arithmetic (replace_signal c s v)
                                          | _ -> failwith "impossible"
                                          end)
                                          sigs in 
                     
  List.iter (CG.remove_edge_e g) delete_edges;
  List.iter (CG.add_edge_e g) new_edges;
  (wire, u_combs @ new_combs, g)

(* Remap ids after combinators have been deleted through optimization passes *)
let remap_ids (ctr: unit -> value) (circuit:circuit) : circuit = 
  let wire, combs, g = circuit in 
  let id_map = List.map (fun c -> let i = ctr() in id_of_combinator c, i) combs in 

  let (~$) c = List.assoc (id_of_conn c) id_map in
  let (~$$) c = List.assoc (id_of_combinator c) id_map in


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
  (wire, new_combs, new_g)

let compile_bexp_to_json ?optimize:(optimize=true) (b: bexp) : json list = 
  let circuits = compile_bexp_to_circuit b in 
  let circuits = 
    if optimize then 
      let vars = vars_in_bexp b in 
      let ctr = create_ctr () in 
      let circuits_opt = List.map (primitive_optimization vars) circuits in
      List.map (remap_ids ctr) circuits_opt  
    else circuits in
  let json_list = json_of_circuits circuits in 
  json_list
