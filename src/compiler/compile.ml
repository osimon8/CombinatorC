open Circuit
open Combinator
open Optimize
open FirstPhase
open Layout
open Utils
open Ast.Bexp
open Ast.Ctree
open Ast.Command
open Composition
open Directive

type compiled_circuit = 
| Abstract of circuit
| Concrete of concrete_circuit

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
 | Pole id -> id, "substation", [] (* small-electric-pole*)
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

      print_endline (Printf.sprintf "o: (%f, %f)" (fst o) (snd o));

      let x c1 c2 rev = 
        let c2, c2_layout = c2 in 
        let p2, s2, pl2 = c2_layout in 
              print_endline (Printf.sprintf "p2: (%f, %f)" (fst p2) (snd p2));

        print_endline @@ string_of_layout c2_layout;
        let o2 = offset o p2 in 
              print_endline (Printf.sprintf "o2: (%f, %f)" (fst o2) (snd o2));

        let l2 = move_layout c2_layout o2 in 
          print_endline @@ string_of_layout l2;

        let p2, s2, pl2 = move_layout c2_layout o2 in 
        

        let l1 = layout c1 in
        let p1, s1, _ = l1 in 
        let mv = if rev then (float_of_int (-(fst s1) - 2), 0.) else (float_of_int (fst (s2) + 2), 0.) in 
        let o1 = offset (offset o p1) mv in 
        let p1, s1, pl1 = move_layout l1 o1 in
              print_endline (Printf.sprintf "o1: (%f, %f)" (fst o1) (snd o1));


        let c = if rev then f c2 c1 else f c1 c2 in
        let o = if rev then p1 else p2 in 
                      print_endline (Printf.sprintf "new o: (%f, %f)" (fst o) (snd o));

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
    end in 

    begin match ctree with 
    | Inline (b, o_sig, loc) -> bind (w o_sig b) loc 
    | Bound (s, loc) -> r (lookup s)
    | Union (b1, b2, loc) -> comp circuit_concat b1 b2 loc 
    | Concat (b1, b2, loc) -> comp circuit_concat b1 b2 loc
  end in 

  let circuit = r ctree in 
  circuit 

let compile_commands_to_circuits ?optimize_b:(optimize_b=true) ?optimize:(optimize=true) (commands: command list) : compiled_circuit list = 
  let table = ref [] in 

  let lookup ident =
    let rec inter table = 
      begin match table with 
      | [] -> prerr_endline @@ "Unknown circuit identifier: \"" ^ ident ^ "\""; exit 1 
      | h :: tl -> let id, b = h in if id = ident then b else inter tl
    end in 
  inter !table in 

  let register ident b o_sig concrete = 
    table := (ident, Inline (b, o_sig, if concrete then Some (get_origin ()) else None)) :: !table
  in

  let compile command = 
    begin match command with 
    | Assign (ident, b, o_sig, concrete) -> register ident b o_sig concrete; None
    | Output c -> Some (compile_ctree_to_circuit ~optimize_b ~optimize lookup c)
    | OutputAt (c, loc) -> Some (compile_ctree_to_circuit ~optimize_b ~optimize lookup (bind_loc c loc))
    end 
  in

  let circuit_opts = List.map compile commands in 
  deopt_list circuit_opts  