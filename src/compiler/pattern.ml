open Ast.Expression
open Ast.Bexp
open Ctxt
open Ast.Combinator
open Ast.Circuit
open FirstPhase

let builtins : (string * pattern) list =
  [
    ("counter", [ (TInt, "max_value"); (TSignal, "o_sig") ]);
    ("counter2", [ (TInt, "max_value"); (TSignal, "i_sig"); (TSignal, "o_sig") ]);
    ("lamp", [ (TCondition, "cnd") ]);
  ]

let register_builtins () = List.iter (fun (s, args) -> Ctxt.add s (TPattern, Pattern args)) builtins

let lamp_config_of_condition (cnd : bexp) : lamp_config * symbol list =
  let sigs = ref [] in
  let bind s = sigs := s :: !sigs in
  let f b2 =
    begin
      match b2 with
      | Signal s ->
          bind s;
          Symbol s
      | Lit l -> Const l
      | _ -> failwith "invalid condition"
    end
  in
  let o1, op, o2 =
    begin
      match cnd with
      | BOOL (Signal s) ->
          bind s;
          (Symbol s, Neq, Const 0l)
      | Not (Signal s) ->
          bind s;
          (Symbol s, Eq, Const 0l)
      | Gt (Signal s, b2) ->
          bind s;
          (Symbol s, Gt, f b2)
      | Gte (Signal s, b2) ->
          bind s;
          (Symbol s, Gte, f b2)
      | Lt (Signal s, b2) ->
          bind s;
          (Symbol s, Lt, f b2)
      | Lte (Signal s, b2) ->
          bind s;
          (Symbol s, Lte, f b2)
      | Eq (Signal s, b2) ->
          bind s;
          (Symbol s, Eq, f b2)
      | Neq (Signal s, b2) ->
          bind s;
          (Symbol s, Neq, f b2)
      | _ -> failwith "invalid condition"
    end
  in
  ({ left_input = o1; op; right_input = o2 }, !sigs)

let evaluate_pattern pattern args : compiled_circuit =
  match pattern with
  | "lamp" ->
      let id = get_entity_id () in
      let _, cnd = Ctxt.lookup "cnd" in
      let cnd =
        match cnd with
        | Condition cnd -> cnd
        | _ -> failwith "invalid argument (not a condition), shouldn't happen"
      in
      let cfg, sigs = lamp_config_of_condition cnd in
      let lamp = Lamp (id, cfg) in
      let origin = get_origin () in
      let layout = (origin, size_of_combinator lamp, [ origin ]) in
      let meta : circuit_meta =
        {
          max_id = id;
          input_sigs = sigs;
          output_sigs = sigs;
          input_ids = [ id ];
          output_ids = [ id ];
        }
      in
      let g = CG.create () in
      let c = ([ lamp ], g, meta) in
      Concrete (c, layout)
  | "counter" ->
      let _, mv = Ctxt.lookup "max_value" in
      let _, o_sig = Ctxt.lookup "o_sig" in
      let mv, o_sig =
        match (mv, o_sig) with
        | Int mv, Signal o_sig -> (mv, o_sig)
        | _ -> failwith "invalid args (shouldn't happen)"
      in
      let id1 = get_entity_id () in
      let id2 = get_entity_id () in
      let cnst = Constant (id1, [ (o_sig, 1l) ]) in
      let ctr = Decider (id2, d_cfg (Symbol o_sig, Lt, Const mv, Symbol o_sig, InpCount)) in
      let meta : circuit_meta =
        {
          max_id = id2;
          input_sigs = [];
          output_sigs = [ o_sig ];
          input_ids = [];
          output_ids = [ id2 ];
        }
      in
      let g = CG.create () in
      connect_primary g (C id1) (Din id2);
      connect_primary g (Dout id2) (Din id2);
      let c = ([ cnst; ctr ], g, meta) in
      Abstract c
  | "counter2" ->
      let _, mv = Ctxt.lookup "max_value" in
      let _, i_sig = Ctxt.lookup "i_sig" in
      let _, o_sig = Ctxt.lookup "o_sig" in
      let mv, i_sig, o_sig =
        match (mv, i_sig, o_sig) with
        | Int mv, Signal i_sig, Signal o_sig -> (mv, i_sig, o_sig)
        | _ -> failwith "invalid args (shouldn't happen)"
      in
      let id1 = get_entity_id () in
      let ctr = Decider (id1, d_cfg (Symbol i_sig, Lt, Const mv, Symbol i_sig, InpCount)) in
      let g = CG.create () in
      connect_primary g (Dout id1) (Din id1);
      let mid, combs, o_conns =
        if i_sig <> o_sig then (
          let id2 = get_entity_id () in
          let map = Arithmetic (id2, a_cfg (Symbol i_sig, Add, Const 0l, Symbol o_sig)) in
          connect_primary g (Dout id1) (Ain id2);
          (id2, [ ctr; map ], [ id2 ]))
        else (id1, [ ctr ], [ id1 ])
      in
      let meta : circuit_meta =
        {
          max_id = mid;
          input_sigs = [ i_sig ];
          output_sigs = [ o_sig ];
          input_ids = [ id1 ];
          output_ids = o_conns;
        }
      in
      let c = (combs, g, meta) in
      Abstract c
  | _ -> failwith "Unknown pattern, should never happen"
