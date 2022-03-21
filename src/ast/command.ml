open Bexp
open Ctree

type command = 
| Assign of string * bexp * string 
| Output of ctree 