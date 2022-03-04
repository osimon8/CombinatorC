open Ast
open Circuit
open Combinator
open Utils

let conns_of_comb (c: combinator) : connection list = 
  begin match c with 
    | Pole i -> [ P i ]
    | Constant (i, _) -> [ C i ]
    | Arithmetic (i, _) -> [ Ain i; Aout i] 
    | Decider (i, _) -> [ Din i; Dout i ]
  end

let conns_of_combs (combs: combinator list) : connection list = 
  List.concat_map conns_of_comb combs

let compile_bexp_to_circuit (b: bexp) : circuit list = 
  let entity_ctr = create_ctr () in
  let rec cb b = 
    begin match b with 
    | Var v -> [ Pole (entity_ctr ()) ] (* lone var just propagates signal (pole) *)
    | Lit l ->  [ Constant (entity_ctr (), [(Green "signal-A", l)]) ] (* lone lit is a constant *)
    | Plus (b1, b2) -> begin match b1, b2 with 
                      | Lit l1, Lit l2 -> [ Arithmetic (entity_ctr (), (Const l1, Add, Const l2, Symbol "signal-0")) ]
                      | Lit l, Var v -> [ Arithmetic (entity_ctr (), (Const l, Add, Symbol v, Symbol "signal-0")) ]
                      | Var v, Lit l -> [ Arithmetic (entity_ctr (), (Symbol v, Add, Const l, Symbol "signal-0")) ]
                      | _ -> failwith "too hard"
                      end
    end 
  in 
  let combs = (Pole (entity_ctr ())) :: (cb b) @ [ Pole (entity_ctr ()) ] in 
  let conns = conns_of_combs combs in 
  let g = CG.create () in 
  let rec wire conns =
    begin match conns with 
    | h1 :: h2 :: tl -> begin match h1 with 
                        | Ain _ | Din _ -> ()
                        | _ -> CG.add_edge g h1 h2
                        end; 
                         wire (h2 :: tl) 
    | _ -> ()
    end
  in 
  wire conns;
  [ (Red [], combs, g) ]



let compile_bexp_to_json (b: bexp) = 
  let circuits = compile_bexp_to_circuit b in 
  json_of_circuits circuits
