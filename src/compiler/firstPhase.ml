open Ast.Bexp
open Ast.Circuit
open Ast.Combinator
open Config
open Utils
open Ctxt 
open Sig_list

let lookup (combs:combinator list) (id:id)  : combinator =
  List.find (fun c -> id_of_combinator c = id) combs 

let conns_of_comb (c: combinator) : connection list = 
  begin match c with 
    | Lamp (i, _) -> [ L i ]
    | Pole (i, _) -> [ P i ]
    | Constant (i, _) -> [ C i ]
    | Arithmetic (i, _) -> [ Ain i; Aout i] 
    | Decider (i, _) -> [ Din i; Dout i ]
  end

let i_conn_of_id (combs: combinator list) (id:id) : connection =
  begin match lookup combs id with 
  | Arithmetic _ -> Ain id 
  | Decider _ -> Din id 
  | Constant _ -> C id 
  | Pole _ -> P id 
  | Lamp _ -> L id
  end

let o_conn_of_id (combs: combinator list) (id:id) : connection =
  begin match lookup combs id with 
  | Arithmetic _ -> Aout id 
  | Decider _ -> Dout id 
  | Constant _ -> C id 
  | Pole _ -> P id 
  | Lamp _ -> L id
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

let connect_product f g c1 c2 = 
  List.iter (fun c1 -> List.iter (f g c1) c2) c1

let detect_signal_collision (output_sig:symbol) (b:bexp) : bool = 
  let rec intern under_conditional b = 
    let it = intern true in  
    let i = intern under_conditional in  
    begin match b with 
    | Conditional (b1, b2, b3) -> it b1 || it b2 || it b3
    | Var _ -> false
    | Lit l -> false 
    | Signal v -> v = output_sig && under_conditional
    | Plus (b1, b2)
    | Minus (b1, b2)
    | Div (b1, b2)
    | Mul (b1, b2)
    | Exp (b1, b2)
    | Mod (b1, b2)
    | Lshift (b1, b2)
    | Rshift (b1, b2)
    | AND (b1, b2)
    | OR (b1, b2)
    | XOR (b1, b2)
    | Gt (b1, b2) 
    | Lt (b1, b2) 
    | Gte (b1, b2) 
    | Lte (b1, b2) 
    | Eq (b1, b2) 
    | Neq (b1, b2) 
    | LAND (b1, b2)
    | LOR (b1, b2)
    | NAND (b1, b2)
    | NOR (b1, b2) -> i b1 || i b2
    | Not b 
    | BOOL b
    | Neg b -> i b
    end in 
  intern false b 

let bind_vars_of_bexp bexp : bexp =  
  let rec i bexp = 
  begin match bexp with 
  | Var var -> 
    let ty, v = Ctxt.lookup var in 
    begin match v with 
    | Int i -> Lit i 
    | Signal s -> Signal s 
    | Condition b -> i b
    | Var v -> i (Var v)
    | _ -> prerr_endline (Printf.sprintf "Can't bind variable \"%s\" of type \"%s\" to expression" var (Ast.Expression.string_of_type ty)) ; exit 1 
    end    
  | Plus (b1, b2) -> Plus (i b1, i b2)
  | Minus (b1, b2) -> Minus (i b1, i b2) 
  | Div (b1, b2) -> Div (i b1, i b2)
  | Mul (b1, b2) -> Mul (i b1, i b2)
  | Exp (b1, b2) -> Exp (i b1, i b2)
  | Mod (b1, b2) -> Mod (i b1, i b2)
  | Lshift (b1, b2) -> Lshift (i b1, i b2)
  | Rshift (b1, b2) -> Rshift (i b1, i b2)
  | AND (b1, b2) -> AND (i b1, i b2)
  | OR (b1, b2) -> OR (i b1, i b2)
  | XOR (b1, b2) -> XOR (i b1, i b2)
  | Gt (b1, b2) -> Gt (i b1, i b2)
  | Lt (b1, b2) -> Lt (i b1, i b2)
  | Gte (b1, b2) -> Gte (i b1, i b2)
  | Lte (b1, b2) -> Lte (i b1, i b2)
  | Eq (b1, b2) -> Eq (i b1, i b2)
  | Neq (b1, b2) -> Neq (i b1, i b2)
  | LAND (b1, b2) -> LAND (i b1, i b2)
  | LOR (b1, b2) -> LOR (i b1, i b2)
  | NAND (b1, b2) -> NAND (i b1, i b2)
  | NOR (b1, b2) -> NOR (i b1, i b2)
  | Conditional (b1, b2, b3) -> Conditional (i b1, i b2, i b3)
  | Not b -> Not (i b)
  | BOOL b -> BOOL (i b)
  | Neg b -> Neg (i b) 
  | Signal _ 
  | Lit _ -> bexp 
  end 
in optimize_bexp (i bexp) 
let circuit_of_bexp (output_sig:symbol) (b: bexp) : circuit = 
  (* print_endline @@ string_of_bexp b; *)
  let b = bind_vars_of_bexp b in
  let vars = signals_in_bexp b in 
  let sigs = output_sig :: vars in

  let sig_ctr = create_sig_ctr sigs in
  let collision = detect_signal_collision output_sig b in 
  let original_out = output_sig in 
  let output_sig = if collision then sig_ctr () else original_out in 

  let g = CG.create () in 

  let connect = 
    connect_product connect_primary g in 
  
  let connect2 = 
    connect_product connect_secondary g in 

  let get_o_sig b = 
    begin match b with 
    | Signal v -> v
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
      let id = get_entity_id () in
      let c, i = 
        begin match b1, b2 with 
          | Lit l1, Lit l2 -> [ Arithmetic (id, (Const l1, aop, Const l2, Symbol o_sig)) ], [] 
          | Lit l, Signal v -> [ Arithmetic (id, (Const l, aop, Symbol v, Symbol o_sig)) ], [id] 
          | Signal v, Lit l -> [ Arithmetic (id, (Symbol v, aop, Const l, Symbol o_sig)) ], [id] 
          | Signal v1, Signal v2 ->[ Arithmetic (id, (Symbol v1, aop, Symbol v2, Symbol o_sig)) ], [id] 
          | _ -> let c1, iids1, o1, s1 = cb b1 in 
                  let c2, iids2, o2, s2 = cb b2 in 
                  let iids = bin_iid_map id iids1 iids2 in 
                connect (o1 @ o2) [Ain id];
                c1 @ c2 @ [ Arithmetic (id, (Symbol s1, aop, Symbol s2, Symbol o_sig)) ], iids
        end in 
    c, Some i, [Aout id], o_sig in

    let d_binop b1 b2 dop = 
      let id = get_entity_id () in
      let c, i = 
        begin match b1, b2 with 
          | Lit l1, Lit l2 -> [ Decider (id, (Const l1, dop, Const l2, Symbol o_sig, One))], []
          | Lit l, Signal v -> [ Decider (id, (Const l, dop, Symbol v, Symbol o_sig, One))], [id]
          | Signal v, Lit l -> [ Decider (id, (Symbol v, dop, Const l, Symbol o_sig, One))], [id]
          | Signal v1, Signal v2 -> [ Decider (id, (Symbol v1, dop, Symbol v2, Symbol o_sig, One))], [id] 
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
      let id1 = get_entity_id () in 
      let id2 = get_entity_id () in 
      let t_sig = sig_ctr () in 
      let iids = bin_iid_map id1 iids1 iids2 in 

      connect (o1 @ o2) [Ain id1];
      connect [Aout id1] [Din id2];
      c1 @ c2 @ [ Arithmetic (id1, (Symbol s1, aop, Symbol s2, Symbol t_sig) );
                  Decider (id2, (Symbol t_sig, dop, Const 0l, Symbol o_sig, One)) ]
      , Some iids, [Dout id2], o_sig
    in

    let isolate b dop dtype = 
      let c, iids, o, s = cb b in 
      let id = get_entity_id () in 
      let iids = unary_iid_map id iids in 
      connect o [Din id];
      c @ [ Decider (id, (Symbol s, dop, Const 0l, Symbol o_sig, dtype)) ], Some iids, [Dout id], o_sig
    in      

    let conditional b1 b2 b3 = 
      let c1, iids1, o1, guard = cb b1 in 
      let c2, iids2, o2, _ = cb ~output_sig:(Some o_sig) b2 in 
      let c3, iids3, o3, _ = cb ~output_sig:(Some o_sig) b3 in 
      let id1 = get_entity_id () in 
      let id2 = get_entity_id () in 
      connect o1 [Din id1; Din id2];
      connect2 o2 [Din id1];
      connect2 o3 [Din id2];
      let niids1 = bin_iid_map id1 iids1 iids2 in 
      let niids2 = bin_iid_map id2 iids1 iids3 in 

      let combs = [Decider (id1, (Symbol guard, Neq, Const 0l, Symbol o_sig, InpCount)); 
                   Decider (id2, (Symbol guard, Eq, Const 0l, Symbol o_sig, InpCount))] in 
      c1 @ c2 @ c3 @ combs, Some (niids1 @ niids2), [Dout id1; Dout id2], o_sig
    in

    let cnst l : combinator list * (id list) option * connection list * symbol = 
      let id = get_entity_id () in 
      [Constant (id, [(o_sig, l)])], Some [], [C id], o_sig (* lone lit is a constant *)
    in 

    begin match b with 
    | Signal v -> 
      if v <> o_sig then 
        let id = get_entity_id () in 
        let combs = [Arithmetic (id, (Symbol v, Add, Const 0l, Symbol o_sig))] in 
        combs, Some [id], [Aout id], o_sig
      else 
      [], None, [], o_sig (* IO wrapping will handle, do nothing.
                           Setting iids to None signals that the upper level needs to be an input   *)
    | Lit l -> cnst l 
    | Neg b -> 
              let c, iids, o, s = cb b in 
              let id = get_entity_id () in 
              let iids = unary_iid_map id iids in 
              connect o [Ain id];
              c @ [ Arithmetic (id, (Symbol s, Mul, Const (-1l), Symbol o_sig)) ], Some iids, [Aout id], o_sig
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
    | Var _ -> failwith "impossible error, shouldn't happen"
    end 
  in 
  
  let combs, iids, o_conns, o_sig = cb ~output_sig:(Some(output_sig)) b in
  let iids = begin match iids with 
  | Some l -> l 
  | None -> [] 
  end in 
  let combs, oids = 
    if collision then 
      let id = get_entity_id () in 
      connect o_conns [Ain id];
      List.rev (Arithmetic (id, (Symbol o_sig, Add, Const 0l, Symbol original_out)) :: combs), 
      [id]
  else 
    combs, List.map id_of_conn o_conns
  in

  let m_id = List.fold_left (fun acc c -> max acc (id_of_combinator c)) Int.min_int combs in

 (combs, g, (m_id, vars, [original_out], iids, oids))
