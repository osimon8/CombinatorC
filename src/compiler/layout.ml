open Ast.Circuit
open Ast.Combinator
open Ast.Expression
open Array
open Config

type grid = bool array array 
let string_of_layout circuit_layout = 
  let (ox,oy), (sx, sy), _ = circuit_layout in 
  "Origin: (" ^ string_of_float ox ^ ", " ^ string_of_float oy ^ ") "
  ^ "Size: (" ^ string_of_int sx ^ ", " ^ string_of_int sy ^ ")"

let poi i j = (float_of_int i, float_of_int j)

let string_of_placement p = 
  let x, y = p in
  "(" ^ (string_of_float x) ^ ", " ^ (string_of_float y) ^ ")"

let conn_length = 9 

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


let place_identity (grid:grid) id (comb: combinator) : placement = 
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
  !p
  (* center_of_size !p (size_of_combinator comb) *)

let layout_identity (c:circuit) : placement list = 
  let combs, _, _ = c in 
  let g = gen_grid () in 
  let f i c = place_identity g i c in 
  List.mapi f combs

let place_naive (grid:grid) (g:connection_graph) (comb: combinator) : placement = 
  let s = size_of_combinator comb in 
  let placements = valid_placements grid s in 
  let i = Random.int (List.length placements) in 
  let p = List.nth placements i in 
  make_placement grid p s;
  p
  (* center_of_size p s *)

let layout_naive (c:circuit) : placement list = 
  let combs, g, _ = c in 
  let grid : grid = gen_grid () in 

  let internal (acc:placement list) (comb:combinator)  = 
    let placements = acc in 
    let p = place_naive grid g comb in 
    p :: placements
  in 

  let placements = List.fold_left internal [] combs in 
  List.rev placements

let get_strategy () =   
  let { layout=l } = get_config () in 
  let strategy =
    begin match l with 
    | Identity -> layout_identity
    | Naive -> layout_naive
  end in
  strategy

let layout ?pos:(pos=(0.,0.)) (c:circuit) : circuit_layout = 
  let strategy = get_strategy () in 
  let placements = strategy c in 
  let ox, oy = pos in 
  let placements = List.map (fun (x, y) -> (x +. ox, y +. oy)) placements in
  let combs, _, _ = c in 
  let zipped = List.combine combs placements in 
  let p_a = List.map (fun (c, (x, y)) -> 
    (* let s = size_of_combinator c in   *)
    (* center_of_size (x +. ox, y +. oy) s)  *)
    (x, y))
    zipped in 
  let p_b = List.map (fun (c, (x, y)) -> 
    let sx, sy = size_of_combinator c in  
    (x +. (float_of_int sx), y +. (float_of_int sy)))
    zipped in 
  let min_x = List.fold_left (fun acc (x,_) -> min acc x) Float.max_float p_a in  
  let min_y = List.fold_left (fun acc (_,y) -> min acc y) Float.max_float p_a in  
  let max_x = List.fold_left (fun acc (x,_) -> max acc x) Float.min_float p_b in  
  let max_y = List.fold_left (fun acc (_,y) -> max acc y) Float.min_float p_b in  

  (min_x, min_y), (int_of_float (max_x -. min_x), int_of_float (max_y -. min_y)), 
  List.map (fun (c,p) -> let s = size_of_combinator c in center_of_size p s) zipped 

let layout_circuits (circuits: circuit list) : circuit_layout list = 
  let inter acc c =
    let pos, size, placements = layout ~pos:(acc) c in 
    let px, py = pos in 
    let sx, sy = size in 
    let sy = max 3 sy in 
    (* print_endline ("POS: (" ^ string_of_float px ^ ", " ^ string_of_float py ^ ")"); *)
    (* print_endline ("SIZE: (" ^ string_of_int sx ^ ", " ^ string_of_int sy ^ ")"); *)

    (px, py +~ sy +. 1.), (pos, size, placements)
  in

  let _, layouts = List.fold_left_map inter (0.,0.) circuits in 
  (* List.map (fun (_, _, p) -> p) layouts *)
  layouts


  let bind_loc ctree (loc:placement) : ctree = 
  let l = Some loc in 
  begin match ctree with 
  | Union(c1, c2, _) -> Union(c1, c2, l)
  | Concat(c1, c2, _) -> Concat(c1, c2, l)
  | Expression(e, _) -> Expression(e, l)
  | Inline(b, s, _) -> Inline(b, s, l)
  | Compiled c -> begin match c with 
                  | Abstract c -> Compiled (Concrete (c, layout ~pos:loc c)) 
                  | Concrete c ->  
                    let c, l = c in 
                    let l = move_layout l loc in 
                    Compiled (Concrete (c, l))
                  end 
  end 