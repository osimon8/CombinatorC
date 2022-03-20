open Circuit 
open Combinator
open Config
open FirstPhase

let circuit_concat (c1:circuit) (c2:circuit) : circuit = 
  let combs1, g1, meta1 = c1 in 
  let m1, i_sigs2, o_sigs1, input1, output1 = meta1 in

  let combs2, g2, meta2 = c1 in 
  let m2, i_sigs1, o_sigs2, input2, output2 = meta2 in

  let new_g = CG_ops.union g1 g2 in 

  let o = List.map (o_conn_of_id combs1) output1 in 
  let i = List.map (i_conn_of_id combs2) input2 in 

  connect_product connect_primary new_g o i;

  (combs1 @ combs2, new_g, 
  (max m1 m2, i_sigs1, o_sigs2, input1, output2))

let circuit_union (c1:circuit) (c2:circuit) : circuit = 
  let combs1, g1, meta1 = c1 in 
  let m1, i_sigs2, o_sigs1, input1, output1 = meta1 in

  let combs2, g2, meta2 = c1 in 
  let m2, i_sigs1, o_sigs2, input2, output2 = meta2 in

  let new_g = CG_ops.union g1 g2 in 

  let f = Core_kernel.List.stable_dedup in 

  let input = f (input1 @ input2) in 
  let output = f (output1 @ output2) in 
  let i_sigs = f (i_sigs1 @ i_sigs2) in 
  let o_sigs = f (o_sigs1 @ o_sigs2) in 

  (combs1 @ combs2, new_g, 
  (max m1 m2, i_sigs, o_sigs, input, output))