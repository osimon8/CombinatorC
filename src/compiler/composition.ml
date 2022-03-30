open Ast.Circuit 
open Ast.Combinator
open Config
open FirstPhase

let circuit_concat (c1:circuit) (c2:circuit) : circuit = 
  let combs1, g1, meta1 = c1 in 
  let m1, i_sigs1, o_sigs1, input1, output1 = meta1 in

  let combs2, g2, meta2 = c2 in 
  let m2, i_sigs2, o_sigs2, input2, output2 = meta2 in

  let new_g = CG_ops.union g1 g2 in 

  let o = List.map (o_conn_of_id combs1) output1 in 
  let i = List.map (i_conn_of_id combs2) input2 in 

  let unmapped_isigs = List.filter (fun s -> not (List.mem s o_sigs1)) i_sigs2 in 

  if unmapped_isigs = i_sigs2 then 
    prerr_endline "WARNING: circuit on left side of concatenation has no output signals matching input signals of circuit on right side";
  let i_sigs = i_sigs1 @ unmapped_isigs in 

  connect_product connect_primary new_g o i;

  (combs1 @ combs2, new_g, 
  (max m1 m2, i_sigs, o_sigs2, input1, output2))

let circuit_union (c1:circuit) (c2:circuit) : circuit = 
  let combs1, g1, meta1 = c1 in 
  let m1, i_sigs1, o_sigs1, input1, output1 = meta1 in

  let combs2, g2, meta2 = c2 in 
  let m2, i_sigs2, o_sigs2, input2, output2 = meta2 in

  let new_g = CG_ops.union g1 g2 in 

  let f = Core.List.stable_dedup in 

  List.iter (fun s -> if List.mem s i_sigs2 then
        prerr_endline "WARNING: circuit on left side of union has output signal that matches an input signal of circuit on right side, this may cause unexpected behavior")
   o_sigs1;

  List.iter (fun s -> if List.mem s i_sigs1 then
        prerr_endline "WARNING: circuit on right side of union has output signal that matches an input signal of circuit on left side, this may cause unexpected behavior")
   o_sigs2;

  let input = f (input1 @ input2) in 
  let output = f (output1 @ output2) in 
  let i_sigs = f (i_sigs1 @ i_sigs2) in 
  let o_sigs = f (o_sigs1 @ o_sigs2) in 

  (combs1 @ combs2, new_g, 
  (max m1 m2, i_sigs, o_sigs, input, output))