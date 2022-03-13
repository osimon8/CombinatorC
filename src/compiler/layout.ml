open Circuit
open Combinator
open Array

type grid = bool array array 
type placement = int * int
let conn_length = 10 

let block_x, block_y = (10, 8) 

let fits_in_block (c:circuit) : bool = 
  let _, combs, _, _ = c in
  List.length combs <= block_x * (block_y / 2)

let gen_grid () : grid = Array.make_matrix conn_length conn_length false

let grid_size grid : size = Array.length (grid), Array.length (grid.(0))

let is_valid_placement (grid:grid) (p:placement) (s:size) : bool = 
  let x, y = p in 
  let sx, sy = s in

  let bx, by = x + sx - 1, y + sy - 1 in 
  let gx, gy = grid_size grid in 

  let valid = ref true in

  for i = x to bx do 
    for j = y to by do 
      valid := !valid && (bx < gx && by < gy) && not (grid.(i).(j))
    done
  done;
  !valid

let valid_placements (grid:grid) (s:size) : placement list = 
  let gx, gy = grid_size grid in 
  let acc = ref [] in 

  for i = 0 to gx - 1 do 
    for j = 0 to gy - 1 do 
      let p = (i, j) in 
      if is_valid_placement grid p s then acc := p :: !acc 
    done
  done;
  !acc

let make_placement (grid:grid) (p:placement) (s:size) : unit = 
  if not (is_valid_placement grid p s) then failwith "attempted placement at filled position";
  let x, y = p in 
  let sx, sy = s in

  for i = x to x + sx - 1 do 
    for j = y to y + sy - 1 do 
      grid.(i).(j) <- true 
    done
  done

let place_identity (comb: combinator) : placement = 
  let id = id_of_combinator comb in 
  (id mod 8, 2 * (id / 8))

let layout_identity (c:circuit) : placement list = 
  let _, combs, _, _ = c in 
  List.map place_identity combs
  (* let intern x y combs = 
    
  in 
  let _, _, pl = List.fold_left intern (0, 0, []) combs in 
  pl *)

let place_naive (grid:grid) (g:connection_graph) (comb: combinator) : placement = 
  let s = size_of_combinator comb in 
  let placements = valid_placements grid s in 
  let i = Random.int (List.length placements) in 
  List.nth placements i

let layout_naive (c:circuit) : placement list = 
  let _, combs, g, _ = c in 
  let grid : grid = gen_grid () in 

  let internal (acc:placement list) (comb:combinator)  = 
    let placements = acc in 
    let p = place_naive grid g comb in 
    p :: placements
  in 

  let placements = List.fold_left internal [] combs in 
  List.rev placements
