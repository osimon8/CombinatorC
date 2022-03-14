open Circuit
open Combinator
open Array
open Config

type grid = bool array array 
type placement = float * float

type circuit_layout = placement * size * placement list 
let poi i j = (float_of_int i, float_of_int j)

let string_of_placement p = 
  let x, y = p in
  "(" ^ (string_of_float x) ^ ", " ^ (string_of_float y) ^ ")"

let conn_length = 10 

let (+~) x y = Float.add x (float_of_int y)
let (+~~) x y = Float.add (float_of_int x) y

let (-~) x y = Float.sub x (float_of_int y)
let (/~) x y = Float.div (float_of_int x) y

let (~~) p = (int_of_float (fst p), int_of_float (snd p))

let center_of_size (p:placement) (s:size) : placement =
  let px, py = p in
  let sx, sy = s in 
  (px +. (sx /~ 2.), py +. (sy /~ 2.)) 

let gen_grid () : grid = 
  let x = conn_length in 
  let y = conn_length in 
  Array.make_matrix x y false

let index grid i j =
  let i, j = ~~(i,j) in 
  grid.(i).(j)

let grid_size grid : size = Array.length (grid), Array.length (grid.(0))

let iter_over_size_in_grid (f: int -> int -> unit) (grid:grid) (p:placement) (s:size) : unit =
  (* let x, y = center_of_size p s in *)
  let x, y = p in 
  let xi, yi = int_of_float x, int_of_float y in
  
  let sx, sy = s in

  let bx, by = xi + sx - 1, yi + sy - 1 in

  for i = xi to bx do 
    for j = yi to by do
      f i j
    done
  done  

let print_coords grid p s = 
  iter_over_size_in_grid (fun i j -> print_endline (string_of_placement (poi i j)))
   grid p s  

let is_valid_placement (grid:grid) (p:placement) (s:size) : bool = 
  let x, y = p in 
  let xi, yi = int_of_float x, int_of_float y in

  let sx, sy = s in
  let bx, by = xi + sx - 1, yi + sy - 1 in 

  let gx, gy = grid_size grid in 

  if (bx >= gx || by >= gy) then false else  
    let valid = ref true in
    let f i j = 
      valid := !valid && not (grid.(i).(j))
    in

    iter_over_size_in_grid f grid p s;
    !valid

let valid_placements (grid:grid) (s:size) : placement list = 
  let acc = ref [] in 

  let f i j =      
    let p = poi i j in 
    if is_valid_placement grid p s then acc := p :: !acc 
  in

  iter_over_size_in_grid f grid (0., 0.) (grid_size grid);
  !acc

let make_placement (grid:grid) (p:placement) (s:size) : unit = 
  if not (is_valid_placement grid p s) then failwith "attempted placement at filled position";

  let f i j = grid.(i).(j) <- true in
  iter_over_size_in_grid f grid p s


let place_identity (grid:grid) (comb: combinator) : placement = 
  let id = id_of_combinator comb in 
  let s = size_of_combinator comb in
  let l = conn_length + 1 in

  let idp id = (float_of_int (id mod l), float_of_int (2 * (id / l))) in 

  let p = ref (idp id) in 
  let i = ref id in 

  while not (is_valid_placement grid !p s) do 
    i := !i + 1;
    p := idp !i 
  done;

  make_placement grid !p s;
  center_of_size !p (size_of_combinator comb)

let layout_identity ?pos:(pos=(0.,0.)) (c:circuit) : placement list = 
  let _, combs, _, _ = c in 
  let g = gen_grid () in 
  let f = place_identity g in 
  List.map f combs

let place_naive (grid:grid) (g:connection_graph) (comb: combinator) : placement = 
  let s = size_of_combinator comb in 
  let placements = valid_placements grid s in 
  let i = Random.int (List.length placements) in 
  let p = List.nth placements i in 
  make_placement grid p s;
  center_of_size p s

let layout_naive ?pos:(pos=(0.,0.)) (c:circuit) : placement list = 
  let _, combs, g, _ = c in 
  let grid : grid = gen_grid () in 

  let internal (acc:placement list) (comb:combinator)  = 
    let placements = acc in 
    let p = place_naive grid g comb in 
    p :: placements
  in 

  let placements = List.fold_left internal [] combs in 
  List.rev placements

let layout ?pos:(pos=(0.,0.)) f (c:circuit) : circuit_layout = 
  let placements = f c in 
  let ox, oy = pos in 
  let p_a = List.map (fun (x, y) -> (x +. ox, y +. oy)) placements in 
  let min_x = List.fold_left (fun acc (x,_) -> min acc x) Float.max_float p_a in  
  let min_y = List.fold_left (fun acc (_,y) -> min acc y) Float.max_float p_a in  
  let max_x = List.fold_left (fun acc (x,_) -> max acc x) Float.min_float p_a in  
  let max_y = List.fold_left (fun acc (_,y) -> max acc y) Float.min_float p_a in  

  (min_x, min_y), (int_of_float (max_x -. min_x), int_of_float (max_y -. min_y)), p_a

let layout_circuits (circuits: circuit list) : placement list list = 
  let { layout=l } = get_config () in 
  let strategy =
    begin match l with 
    | Identity -> layout_identity
    | Naive -> layout_naive
    end
in

  let inter acc c =
    let pos, size, placements = layout ~pos:(acc) strategy c in 
    let px, py = pos in 
    let _, sy = size in 
    (px, py +~ sy +. 1.), (pos, size, placements)
  in

  let _, layouts = List.fold_left_map inter (0.,0.) circuits in 
  List.map (fun (_, _, p) -> p) layouts