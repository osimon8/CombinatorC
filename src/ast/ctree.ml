open Bexp

type ctree = 
| Union of ctree * ctree 
| Concat of ctree * ctree 
| Bound of string 
| Inline of bexp * string    