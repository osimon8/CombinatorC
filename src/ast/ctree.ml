open Bexp

type loc = (float * float ) option

type ctree = 
| Union of ctree * ctree * loc
| Concat of ctree * ctree * loc 
| Bound of string * loc
| Inline of bexp * string * loc

let bind_loc ctree (l:float*float) : ctree = 
  let l = Some l in 
  begin match ctree with 
  | Union(c1, c2, _) -> Union(c1, c2, l)
  | Concat(c1, c2, _) -> Concat(c1, c2, l)
  | Bound(s, _) -> Bound(s, l)
  | Inline(b, s, _) -> Inline(b, s, l)
  end 

let rec is_concrete ctree : bool =
  let c loc = match loc with | Some _ -> true | None -> false in 
  begin match ctree with 
  | Union(c1, c2, loc)  
  | Concat(c1, c2, loc) -> c loc || is_concrete c1 || is_concrete c2
  | Bound(_, loc) -> c loc
  | Inline(_, _, loc) -> c loc
  end 