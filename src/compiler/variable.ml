open Ast.Bexp

type variable = 
| Int of int32
| Bool of bool
| Condition of bexp  
| Signal of string 

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