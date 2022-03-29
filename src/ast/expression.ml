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

let signal_or_var bexp : bool = 
  begin match bexp with 
  | Signal _ 
  | Var _ -> true 
  | _ -> false 
  end 

let valid_condition bexp : bool = 
  begin match bexp with 
  | Gt (b1, b2) 
  | Gte (b1, b2) 
  | Lt (b1, b2) 
  | Lte (b1, b2) 
  | Eq (b1, b2) 
  | Neq (b1, b2) -> signal_or_var b1 && 
                      begin match b2 with 
                        | Lit _ -> true 
                        | _ -> signal_or_var b2  
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
| For of bool * string * int32 * int32 * bool * command list
and ctree = 
| Union of ctree * ctree * loc
| Concat of ctree * ctree * loc 
| Expression of expression * loc
| Compiled of compiled_circuit
| Inline of bexp * string * loc
and command = 
| CircuitBind of string * bexp * string * bool
| Assign of string * var_type * expression
| Output of expression
| OutputAt of expression * (bexp * bexp)

type block = command list 

let num_outputs (block:block) : int =
  let vali acc c = 
    begin match c with 
    | Output _
    | OutputAt _ -> acc + 1
    | _ -> acc 
    end 
  in
  List.fold_left vali 0 block 

let expression_of_bexp (bexp:bexp) : expression = 
  let lit_opt = interpret_bexp bexp in 
  begin match lit_opt with 
  | Some l -> Int l 
  | None -> 
    begin match bexp with 
    | Signal s -> Signal s 
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

let rec is_concrete ctree : bool =
  let c loc = match loc with | Some _ -> true | None -> false in 
  begin match ctree with 
  | Union(c1, c2, loc)  
  | Concat(c1, c2, loc) -> c loc || is_concrete c1 || is_concrete c2
  | Expression(_, loc) -> c loc
  | Inline(_, _, loc) -> c loc
  | Compiled c -> begin match c with 
                  | Concrete _ -> true 
                  | Abstract _ -> false 
                  end 
  end 