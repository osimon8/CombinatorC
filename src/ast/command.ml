open Bexp
open Ctree

type loc = (float * float) 

type command = 
| Assign of string * bexp * string * bool
| Output of ctree
| OutputAt of ctree * loc