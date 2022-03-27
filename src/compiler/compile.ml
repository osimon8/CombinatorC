open Ast.Circuit
open Ast.Combinator
open Optimize
open FirstPhase
open Layout
open Utils
open Ast.Bexp
open Ast.Command
open Ast.Expression
open Composition
open Directive
open Pattern
open Ctxt

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

let compile_bexp_to_circuit ?optimize:(optimize=true) (o_sig:symbol) (b: bexp) : circuit = 
  let circuit = circuit_of_bexp o_sig b in 
    if optimize then 
      let circuit_opt = primitive_optimization circuit in
      circuit_opt
    else 
      circuit 

let json_of_bexp ?optimize:(optimize=true) (o_sig:symbol) (b: bexp) : json list = 
  let circuit = compile_bexp_to_circuit ~optimize o_sig b in 
  json_of_circuits [circuit]


let evaluate_call (pattern:string) (args: (var_type * expression) list) : compiled_circuit = 
  let p_args = Ctxt.lookup_pattern pattern in 
  let lp = List.length p_args in 
  let l = List.length args in 
  if lp <> l then (prerr_endline @@ Printf.sprintf "Pattern \"%s\" expects %d arguments, got %d" pattern lp l; exit 1);
  
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

let interpret_types (exps: expression list) : (var_type * expression) list = 
  let rec inter exp =
    begin match exp with 
    | Int _ -> TInt 
    | Condition _ -> TCondition
    | Signal _ -> TSignal
    | Circuit _  
    | Call _ -> TCircuit
    | Pattern _ -> TPattern
    | Var s -> let t, _ = Ctxt.lookup s in t 
    end, exp
  in 
  List.map inter exps

let compile_ctree_to_circuit ?optimize_b:(optimize_b=true) ?optimize:(optimize=true) (lookup: string -> ctree) (ctree:ctree) : compiled_circuit = 
  let w o_sig bexp = let ast = 
                if optimize_b then optimize_bexp bexp else bexp in 
              compile_bexp_to_circuit ~optimize o_sig ast in 

  let bind circuit loc =
    begin match loc with 
    | Some pos -> Concrete (circuit, layout ~pos circuit)
    | None -> Abstract circuit
  end in

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
        let mv = if rev then (float_of_int (-(fst s1) - 2), 0.) else (float_of_int (fst (s2) + 2), 0.) in 
        let o1 = offset (offset o p1) mv in 
        let p1, s1, pl1 = move_layout l1 o1 in

        let c = if rev then f c2 c1 else f c1 c2 in
        let o = if rev then p1 else p2 in 

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
    | Inline (b, o_sig, loc) -> bind (w o_sig b) loc
    | Union (b1, b2, loc) -> comp circuit_union b1 b2 loc 
    | Concat (b1, b2, loc) -> comp circuit_concat b1 b2 loc
    (* TODO: use loc *)
    | Expression (e, loc) -> 
        begin match e with
        | Call (p, args) -> evaluate_call p (interpret_types args)
        | Circuit c -> r c 
        | Var v -> r (lookup v)
        | Condition b -> r (Inline (b, "check", loc))
        | Int i -> r (Inline (Lit i, "check", loc))
        | Signal s -> r (Inline (Var s, s, loc))
        | Pattern _ -> prerr_endline @@ "Can't output pattern! How did you manage to do this?"; exit 1
        end 
    end
  
  in 

  let circuit = r ctree in 
  circuit 

let evaluate_expression ?optimize_b:(optimize_b=true) ?optimize:(optimize=true) (lookup: string -> ctree) exp loc : compiled_circuit =
  let rec inter exp = 
      begin match exp with 
      | Call _ -> Expression (exp, loc)
      | Signal s -> Inline (Var s, s, loc)
      | Int i -> Inline (Lit i, "check", loc)
      | Condition b -> Inline (b, "check", loc)
      | Circuit c -> c
      | Var v -> let ty, e = Ctxt.lookup v in 
                begin match ty with 
                | TPattern -> prerr_endline @@ "Can't output pattern :\"" ^ v ^"\""; exit 1
                | _ -> inter e 
                end 
      | Pattern _ -> prerr_endline @@ "Can't output pattern! How did you manage to do this?"; exit 1
      end in 
  let ctree = inter exp in 
  begin match loc with 
  | Some loc -> compile_ctree_to_circuit ~optimize_b ~optimize lookup (bind_loc ctree loc)
  | None -> compile_ctree_to_circuit ~optimize_b ~optimize lookup ctree 
  end 

let compile_commands_to_circuits ?optimize_b:(optimize_b=true) ?optimize:(optimize=true) (commands: command list) : compiled_circuit list = 
  register_builtins ();
  let register ident b o_sig concrete = 
    Ctxt.add ident (TCircuit, Circuit (Inline (b, o_sig, if concrete then Some (get_origin ()) else None)))
  in

  let lookup ident = begin match Ctxt.lookup ident with 
                     | TCircuit, c -> begin match c with 
                                      | Circuit c -> c 
                                      | _ -> failwith "impossible"
                                      end
                     | _ -> prerr_endline @@ "Variable with identifier \"" ^ ident ^ "\" is not a circuit, cannot output"; exit 1
                    end in

  let compile command = 
    begin match command with 
    | CircuitBind (ident, b, o_sig, concrete) -> register ident b o_sig concrete; None
    | Assign (ident, ty, exp) -> Ctxt.add_no_dup ident (ty,exp); None
    | Output e -> Some (evaluate_expression ~optimize_b ~optimize lookup e None)
    | OutputAt (e, loc) -> Some (evaluate_expression ~optimize_b ~optimize lookup e (Some loc))
    end 
  in

  let circuit_opts = List.map compile commands in 
  deopt_list circuit_opts  