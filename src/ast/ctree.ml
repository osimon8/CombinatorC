open Bexp

type ctree = 
| Union of ctree * ctree 
| Concat of ctree * ctree 
| Bexp of string * bexp    