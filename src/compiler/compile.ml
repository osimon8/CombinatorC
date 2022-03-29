open Ast.Circuit
open Ast.Combinator
open Optimize
open FirstPhase
open Layout
open Utils
open Ast.Bexp
open Ast.Expression
open Composition
open Directive
open Pattern
open Ctxt
open Config

let json_of_symbol s = 
  `Assoc [("type", `String "virtual"); ("name", `String ("signal-" ^ s))]

let json_of_value v =
  `Intlit (Int32.to_string v)

let json_of_config (cfg:cfg) : string * json = 
  let mk_sig pre s = (pre ^ "_signal", json_of_symbol s) in

  let parse_ao (pre:string) (o:aop) = 
    begin match o with 
    | Symbol s -> mk_sig pre s
    | Const c -> (pre ^ "_constant", json_of_value c)
    | Each -> mk_sig pre "each"
    end 
  in 

  let parse_do (pre:string) (o:dop) i = 
    begin match o with 
    | Const c -> if i <> 1 then failwith "illegal argument to decider combinator"
                else ("constant", json_of_value c)
    | Symbol s -> mk_sig pre s
    | Each -> mk_sig pre "each"
    | Anything -> mk_sig pre "anything"
    | Everything -> mk_sig pre "everything"
    end
  in 

  let parse_lo (pre:string) (o:dop) i = 
    begin match o with 
    | Const c -> if i <> 1 then failwith "illegal argument to lamp"
                else ("constant", json_of_value c)
    | Symbol s -> mk_sig pre s
    | Each -> failwith "illegal argument to lamp"
    | Anything -> mk_sig pre "anything"
    | Everything -> mk_sig pre "everything"
    end in

  let c_map (i:int) (data:data) =  
    let s, v = data in
    `Assoc [("signal", json_of_symbol s); 
            ("count", json_of_value v); 
            ("index", `Int (i + 1))] in

  ("control_behavior", `Assoc (
  begin match cfg with 
  | A (o1, op, o2, out) ->  [("arithmetic_conditions", `Assoc
                                [(parse_ao "first" o1); (parse_ao "second" o2); 
                                ("operation", `String (string_of_arithmetic_op op)); 
                                (parse_ao "output" out)] 
                              )]
  | D (o1, op, o2, out, t) -> [("decider_conditions", `Assoc
                                ([(parse_do "first" o1 0); (parse_do "second" o2 1); 
                                ("comparator", `String (string_of_decider_op op)); 
                                (parse_do "output" out 2)]  
                                @ (begin match t with 
                                    | One -> [("copy_count_from_input", `Bool false)]
                                    | InpCount -> []
                                    end))
                              )] 
  | C cfg -> [("filters", `List (List.mapi c_map cfg))]
  | L (o1, op, o2) -> [("circuit_condition", `Assoc ([(parse_lo "first" o1 0); 
                                                      (parse_lo "second" o2 1); 
                                                      ("comparator", `String (string_of_decider_op op))]))]
  end))

let json_of_conn = 
  fun (clist:CG.edge list) : (string * json) list ->  
    let red, green = List.partition (fun (_, color, _) -> 
                                      begin match color with 
                                      | Red -> true
                                      | Green -> false 
                                      end) clist in 


    let map_conns l = List.map (fun (_, _, c) -> `Assoc [("entity_id", `Int (id_of_conn c));
                                                        ("circuit_id", `Int (type_id_of_conn c))]) l in 


    let rl, gl = List.length red, List.length green in 
    
    begin match rl, gl with 
    | 0, 0 ->  []
    | _, 0 ->  [(string_of_wire_color Red, `List (map_conns red)) ] 
    | 0, _ ->  [(string_of_wire_color Green, `List (map_conns green)) ] 
    | _, _ ->  [(string_of_wire_color Red, `List (map_conns red));
                     (string_of_wire_color Green, `List (map_conns green)) ] 
    end 

let json_of_combinator (c: combinator) (g: connection_graph) (p:placement) : json = 
  let id, name, cfg_json = begin match c with
 | Arithmetic (id, cfg) -> id, "arithmetic-combinator", [json_of_config (A cfg)]
 | Decider (id, cfg) ->  id, "decider-combinator", [json_of_config (D cfg)]
 | Constant (id, cfg) -> id, "constant-combinator", [json_of_config (C cfg)]
 | Lamp (id, cfg) -> id, "small-lamp", [json_of_config (L cfg)] 
 | Pole (id, t) -> id, 
    begin match t with 
    | Small -> "small-electric-pole"
    | Medium -> "medium-electric-pole"
    | Big -> "big-electric-pole"
    | Substation -> "substation"
    end, [] 
  end in 

  let joc label clist = 
    begin match json_of_conn clist with 
    | [] -> [] 
    | l -> [(label, `Assoc l)]
  end in 

  let conns = [("connections", begin match c with 
 | Arithmetic _ -> `Assoc (joc "1" (succs g (Ain id)) @ joc "2" (succs g (Aout id)))
 | Decider _ ->  `Assoc (joc "1" (succs g (Din id)) @ joc "2" (succs g (Dout id)))
 | Constant _-> `Assoc (joc "1" (succs g (C id)))
 | Lamp _ -> `Assoc (joc "1" (succs g (L id)))
 | Pole _ -> `Assoc (joc "1" (succs g (P id)))
  end 
  )] in 

  let x, y = p in
  `Assoc ([("entity_number", `Int id); 
          ("name", `String name);
          ("position", `Assoc [("x", `Float x); ("y", `Float y)]);
          ] @ cfg_json @ conns)

 let json_of_circuit (circuit: concrete_circuit) : json list = 
  let (combs, g, _), (_, _, placements) = circuit in 

  let zipped = List.combine combs placements in
  let json_list = List.map (fun (c, p) -> json_of_combinator c g p) zipped in
  json_list

let json_of_circuits (circuits: circuit list) : json list = 
  let layouts = layout_circuits circuits in 
  let concrete_circuits = List.combine circuits layouts  in
  let wrapped = List.map wrap_io concrete_circuits in 
  let remapped = remap_ids_concrete wrapped in 
  List.concat_map json_of_circuit remapped 

let json_of_compiled_circuits (circuits: compiled_circuit list) : json list = 
  let abstract, concrete = List.partition (fun c -> match c with | Abstract _ -> true | _ -> false) circuits in 
  let abstract = List.map (fun c -> match c with | Abstract c -> c | _ -> failwith "impossible") abstract in 
  let layouts = layout_circuits abstract in 
  let concrete_layouts = List.map (fun c -> match c with | Concrete c -> c | _ -> failwith "impossible") concrete in 
  let concrete_circuits = List.combine abstract layouts @ concrete_layouts  in
  let wrapped = List.map wrap_io concrete_circuits in 
  let remapped = remap_ids_concrete wrapped in 
  List.concat_map json_of_circuit remapped 

let compile_bexp_to_circuit (o_sig:symbol) (b: bexp) : circuit = 
  let circuit = circuit_of_bexp o_sig b in 
    if get_optimize () then 
      let circuit_opt = primitive_optimization circuit in
      circuit_opt
    else 
      circuit 

let json_of_bexp (o_sig:symbol) (b: bexp) : json list = 
  let circuit = compile_bexp_to_circuit o_sig b in 
  json_of_circuits [circuit]

let interpret_types (exps: expression list) : (var_type * expression) list = 
  let rec inter exp =
    begin match exp with 
    | Int _ -> TInt 
    | Condition _ -> TCondition
    | Signal _ -> TSignal
    | Circuit _ 
    | For _ 
    | Call _ -> TCircuit
    | Pattern _ -> TPattern
    | Var s -> let t, _ = Ctxt.lookup s in t 
    end, exp
  in 
  List.map inter exps

let rec bottom_out_var e : expression = 
  begin match e with
  | Var v -> let _, v = Ctxt.lookup v in 
            bottom_out_var v 
  | _ -> e 
  end 


let rec evaluate_expression (expression:expression) : expression = 
  let rec inter exp = 
      begin match exp with 
      | Call (p, args) -> Circuit (Compiled (evaluate_call p args))
      | Condition b -> Condition (bind_vars_of_bexp b)
      | Var v -> bottom_out_var exp 
      | For (concat, id, l, u, count_down, block) -> 
          let n_out = num_outputs block in 
          if n_out = 0 then (prerr_endline "For expression missing output!"; exit 1) 
          else (if n_out > 1 then (prerr_endline "For expression has multiple outputs, must have exactly 1"; exit 1));
          Ctxt.add id (TInt, Int l);
          let c = evaluate_for concat id u count_down block in 
          Circuit (Compiled c) 
      | Signal _ 
      | Int _ 
      | Circuit _
      | Pattern _ -> exp
      end in 
    inter expression 

and evaluate_call (pattern:string) (args: expression list) : compiled_circuit = 
  let p_args = Ctxt.lookup_pattern pattern in 
  let args = interpret_types args in
  let lp = List.length p_args in 
  let l = List.length args in 
  if lp <> l then (prerr_endline @@ Printf.sprintf "Pattern \"%s\" expects %d arguments, got %d" pattern lp l; exit 1);
  
  let args = List.map (fun (ty, a) -> 
    let a = evaluate_expression a in 
    ty, a
    ) args in 

  let zipped = List.combine p_args args in 
  let old_bindings = Ctxt.get () in 
  List.iter (fun (p,a) -> 
    let pty, pname = p in 
    let aty, exp = a in 
    if pty <> aty then (prerr_endline (Printf.sprintf "Pattern \"%s\" expects argument \"%s\" to have type \"%s\", but an argument of type \"%s\" was provided"  
                                                      pattern pname (string_of_type pty) (string_of_type aty)); exit 1) 
    else (Ctxt.add pname (pty, exp)) ) zipped; 
  let circuit = evaluate_pattern pattern args in 
  Ctxt.set old_bindings;
  circuit 
and compile_ctree_to_circuit (ctree:ctree) : compiled_circuit = 
  let w o_sig bexp = 
    let ast = 
    if get_optimize_b () then optimize_bexp bexp else bexp in 
    begin match bexp with
    | Var i -> 
      let e = bottom_out_var (Var i) in 
      begin match e with 
      | Signal _ 
      | Int _ -> Abstract (compile_bexp_to_circuit o_sig ast)
      | Circuit c -> compile_ctree_to_circuit c 
      | _ -> failwith "can't BIND!" 
      end
    | _ -> Abstract (compile_bexp_to_circuit o_sig ast)
    end
    in 

  (* let bind circuit loc =
    begin match loc with 
    | Some pos -> Concrete (circuit, layout ~pos circuit)
    | None -> Abstract circuit
  end in *)

  let rec r ctree = 
    let comp f c1 c2 loc = 
      let c1 = r c1 in 
      let c2 = r c2 in 
      let o = 
        begin match loc with 
        | Some l -> l 
        | None -> get_origin ()
      end in 
      let x c1 c2 rev = 
        let c2, c2_layout = c2 in 
        let p2, s2, pl2 = c2_layout in 
        let o2 = offset o p2 in 
        let p2, s2, pl2 = move_layout c2_layout o2 in 

        let l1 = layout c1 in
        let p1, s1, _ = l1 in 
        let mv = if rev then (float_of_int (fst (s2) + 2), 0.) else (float_of_int (-(fst s1) - 2), 0.) in 
        let o1 = offset (offset o p1) mv in 
        let p1, s1, pl1 = move_layout l1 o1 in


        let c = if rev then f c2 c1 else f c1 c2 in
        let o = if rev then p2 else p1 in 

        Concrete (c, (o, (fst s1 + fst s2 + 2, snd s1 + snd s2), if rev then pl2 @ pl1 else pl1 @ pl2))
      in 


      begin match c1, c2 with 
      | Abstract c1, Abstract c2 -> 
          let c = f c1 c2 in 
          begin match loc with
          | Some pos -> Concrete (c, layout ~pos c)
          | None -> Abstract c
          end 
      | Abstract c1, Concrete c2 -> x c1 c2 false
      | Concrete c1, Abstract c2 -> x c2 c1 true
      | Concrete c1, Concrete c2 -> 
        let c1, c1_layout = c1 in 
        let c2, c2_layout = c2 in 
        let p1, _, _ = c1_layout in 
        let p2, _, _ = c2_layout in 
        let o1 = offset o p1 in 
        let o2 = offset o p2 in 
        let p1, s1, pl1 = move_layout c1_layout o1 in 
        let p2, s2, pl2 = move_layout c2_layout o2 in
        let c = f c1 c2 in 
        Concrete (c, (o, (max (fst s1) (fst s2), max (snd s1) (snd s2)), pl1 @ pl2))
    end 
  in 

    begin match ctree with 
    | Compiled c -> c 
    | Inline (b, o_sig, loc) -> let c = w o_sig b in
                               begin match loc with 
                                | Some loc -> compile_ctree_to_circuit (bind_loc (Compiled c) loc)
                                | None -> c
                                end 
    | Union (b1, b2, loc) -> comp circuit_union b1 b2 loc 
    | Concat (b1, b2, loc) -> comp circuit_concat b1 b2 loc
    | Expression (e, loc) -> evaluate_expression_to_ctree  e loc 
    end
  in 

  let circuit = r ctree in 
  circuit 
and evaluate_expression_to_ctree exp loc : compiled_circuit =
  let exp = evaluate_expression exp in 
  let rec inter exp = 
      begin match exp with 
      | Signal s -> Inline (Signal s, s, loc)
      | Int i -> Inline (Lit i, "check", loc)
      | Condition b -> Inline (b, "check", loc)
      | Circuit c -> c
      | Pattern _ -> prerr_endline "Can't evaluate pattern to circuit. Did you mean to provide arguments?"; exit 1
      | _ -> failwith "Expression not reduced! Impossible error"
      end in 

  let ctree = inter exp in 
  begin match loc with 
  | Some loc -> compile_ctree_to_circuit (bind_loc ctree loc)
  | None -> compile_ctree_to_circuit ctree 
  end 
and evaluate_for (concat: bool) (id:string) (bound:int32) (count_down:bool) (block:block) : compiled_circuit =
  let _, i = Ctxt.lookup id in 
  let i = begin match i with 
  | Int i -> Int32.to_int i 
  | _ -> failwith "Invalid for ctr type, shouldn't happen"
  end in 
  let bound = Int32.to_int bound in 
  let finished i = (i < bound && count_down) || (i > bound && not count_down) in 
  if finished i then 
    (prerr_endline "For loop bounds result in 0 executions of loop, no output circuit"; exit 1);

  let cmp () = Compiled (List.nth (compile_block_to_circuits block) 0) in 
  let update_ctr i = 
    Ctxt.remove id; 
    let i = if count_down then i - 1 else i + 1 in 
    Ctxt.add id (TInt, Int (Int32.of_int i));
    i
  in

  let rec eval prev i = 
    if finished i then prev else 
    let c = cmp () in 
    let c = if concat then Concat(prev, c, None) else Union(prev, c, None) in 
    eval c (update_ctr i)
  in
  let first = cmp () in 
  let c = eval first (update_ctr i) in 
  compile_ctree_to_circuit c
  
and compile_block_to_circuits (commands: block) : compiled_circuit list = 
  let register ident b o_sig concrete = 
    Ctxt.add ident (TCircuit, Circuit (Inline (b, o_sig, if concrete then Some (get_origin ()) else None)))
  in
  let compile command = 
    begin match command with 
    | CircuitBind (ident, b, o_sig, concrete) -> register ident b o_sig concrete; None
    | Assign (ident, ty, exp) -> Ctxt.add_no_dup ident (ty,exp); None
    | Output e -> Some (evaluate_expression_to_ctree e None)
    | OutputAt (e, (e1, e2)) -> 
      let e1 = bind_vars_of_bexp e1 in 
      let e2 = bind_vars_of_bexp e2 in 
      begin match e1, e2 with 
      | Lit v1, Lit v2 -> 
        let loc = (Int32.to_float v1, Int32.to_float v2) in 
        Some (evaluate_expression_to_ctree e (Some loc))
      | _ -> prerr_endline @@ "Output location doesn't map to tuple of ints"; exit 1
      end
    end 
  in

  let circuit_opts = List.map compile commands in 
  deopt_list circuit_opts  

let compile (commands: block) : compiled_circuit list = 
  register_builtins ();
  compile_block_to_circuits  commands 