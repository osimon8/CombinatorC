open Circuit
open Combinator
open Layout
open Utils

let json_of_symbol s = 
  `Assoc [("type", `String "virtual"); ("name", `String ("signal-" ^ s))]

let json_of_config (cfg:cfg) : string * json = 
  let mk_sig pre s = (pre ^ "_signal", json_of_symbol s) in

  let parse_ao (pre:string) (o:aop) = 
    begin match o with 
    | Symbol s -> mk_sig pre s
    | Const c -> (pre ^ "_constant", `Int c)
    | Each -> mk_sig pre "each"
    end 
  in 

  let parse_do (pre:string) (o:dop) i = 
    begin match o with 
    | Const c -> if i <> 1 then failwith "illegal argument to decider combinator"
                else ("constant", `Int c)
    | Symbol s -> mk_sig pre s
    | Each -> mk_sig pre "each"
    | Anything -> mk_sig pre "anything"
    | Everything -> mk_sig pre "everything"
    end
  in 

  let c_map (i:int) (data:data) =  
    let s, v = data in
    (* let s : string = begin match s with Red s | Green s -> s end in *)
    `Assoc [("signal", json_of_symbol s); 
            ("count", `Int v); 
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

let json_of_conn color id = 
  fun (clist:connection list) : json ->  
     `Assoc [(color, `List (List.map (fun c -> `Assoc [("entity_id", `Int (id_of_conn c));
                                                        ("circuit_id", `Int (type_id_of_conn c))])
      clist))]

let json_of_combinator (c: combinator) (wire: wire) (g: connection_graph) (p:placement) : json = 
  let id, name, cfg_json = begin match c with
 | Arithmetic (id, cfg) -> id, "arithmetic-combinator", [json_of_config (A cfg)]
 | Decider (id, cfg) ->  id, "decider-combinator", [json_of_config (D cfg)]
 | Constant (id, cfg) -> id, "constant-combinator", [json_of_config (C cfg)]
 | Pole id -> id, "substation", [] (* small-electric-pole*)
  end in 

  let color = begin match wire with 
  | Red _ -> "red"
  | Green _ -> "green"
  end in

  let joc = json_of_conn color id in

  let conns = [("connections", begin match c with 
 | Arithmetic _ -> `Assoc [("1", joc (succs g (Ain id))); ("2", joc (succs g (Aout id)))]
 | Decider _ ->  `Assoc [("1", joc (succs g (Din id))); ("2", joc (succs g (Dout id)))]
 | Constant _-> `Assoc [("1", joc (succs g (C id)))]
 | Pole _ -> `Assoc [("1", joc (succs g (P id)))]
  end 
  )] in 

  let x, y = p in
  `Assoc ([("entity_number", `Int id); 
          ("name", `String name);
          ("position", `Assoc [("x", `Float x); ("y", `Float y)]);
          ] @ cfg_json @ conns)

 let json_of_circuit (id: id) (circuit: circuit) : json list = 
  let wire, combs, g, _ = circuit in 
  (* let placements = layout_naive circuit in  *)
  let placements = layout_identity circuit in 
  let zipped = List.combine combs placements in

  let json_list = List.map (fun (c, p) -> json_of_combinator c wire g p) zipped in
  json_list

let json_of_circuits (circuits: circuit list) : json list = 
  let red, green = List.partition (fun (w, _, _, _) -> begin match w with
                                  | Red _ -> true | _ -> false end) circuits in
  let rm, gm = List.mapi json_of_circuit red, List.mapi json_of_circuit green in
  List.flatten (rm @ gm)