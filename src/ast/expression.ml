open Bexp
open Circuit

type var_type = 
| TInt
| TCondition
| TSignal
| TCircuit
| TPattern

let string_of_type ty : string = 
  begin match ty with 
  | TInt -> "int"
  | TCondition -> "condition"
  | TSignal -> "signal"
  | TCircuit -> "circuit"
  | TPattern -> "pattern"
  end

let valid_condition bexp : bool = 
  begin match bexp with 
  | Gt (Var s1, b2) 
  | Gte (Var s1, b2) 
  | Lt (Var s1, b2) 
  | Lte (Var s1, b2) 
  | Eq (Var s1, b2) 
  | Neq (Var s1, b2) -> begin match b2 with 
                        | Var _ 
                        | Lit _ -> true 
                        | _ -> false 
                        end
  | _ -> false
  end

type compiled_circuit = 
| Abstract of circuit
| Concrete of concrete_circuit

type expression = 
| Call of string * expression list 
| Int of int32
| Condition of bexp  
| Var of string 
| Signal of string 
| Circuit of ctree
| Pattern of (var_type * string) list
and ctree = 
| Union of ctree * ctree * loc
| Concat of ctree * ctree * loc 
| Expression of expression * loc
| Inline of bexp * string * loc

let expression_of_bexp (bexp:bexp) : expression = 
  let lit_opt = interpret_bexp bexp in 
  begin match lit_opt with 
  | Some l -> Int l 
  | None -> 
    begin match bexp with 
    | Var s -> Signal s 
    | _ -> if valid_condition bexp then Condition bexp else 
            Circuit (Inline (bexp, "check", None))
    end 
  end  

let offset (origin:placement) (off:placement) : placement = 
  let x, y = origin in 
  let ox, oy = off in 
  (ox +. x, oy +. y) 

let move_layout (l:circuit_layout) (new_p:placement) : circuit_layout = 
  let p, s, pl = l in 
  let pl2 = List.map (offset new_p) pl in 
  new_p, s, pl2 



let bind_loc ctree (loc:placement) : ctree = 
  let l = Some loc in 
  begin match ctree with 
  | Union(c1, c2, _) -> Union(c1, c2, l)
  | Concat(c1, c2, _) -> Concat(c1, c2, l)
  | Expression(e, _) -> Expression(e, l)
  | Inline(b, s, _) -> Inline(b, s, l)
  end 

let rec is_concrete ctree : bool =
  let c loc = match loc with | Some _ -> true | None -> false in 
  begin match ctree with 
  | Union(c1, c2, loc)  
  | Concat(c1, c2, loc) -> c loc || is_concrete c1 || is_concrete c2
  | Expression(_, loc) -> c loc
  | Inline(_, _, loc) -> c loc
  end 