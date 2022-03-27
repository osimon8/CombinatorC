open Bexp
open Expression

type loc = (float * float) 

type command = 
| CircuitBind of string * bexp * string * bool
| Assign of string * var_type * expression
| Output of expression
| OutputAt of expression * loc