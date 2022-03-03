type symbol = 
  | Red of string
  | Green of string

type id = int

type value = int

type data = symbol * value

type signal = data list

type wire = 
 | Red of signal 
 | Green of signal

type arithemtic_op = 
 | Add
 | Sub
 | Mul
 | Div
 | Mod
 | Exp
 | Lshift
 | Rshift
 | AND
 | OR
 | XOR

 type decider_op = 
 | Gt
 | Lt
 | Gte
 | Lte
 | Eq
 | Neq

type decider_output = 
 | One
 | InpCount

type aop = 
 | Symbol 
 | Const of int
 | Each

 type dop = 
 | Symbol
 | Const of int
 | Each
 | Anything
 | Everything

(* left input * operation * right input * output  *)
type arithemtic_config = aop * arithemtic_op * aop * aop 

(* left input * operation * right input * output  *)
type decider_config = dop * decider_op * dop * dop 

type constant_config = wire list

type combinator = 
 | Arithmetic of id * arithemtic_config
 | Decider of id * decider_config
 | Constant of id * constant_config
 | Pole of id

type connection = 
  | Ain of id
  | Aout of id
  | Din of id
  | Dout of id
  | C of id
  | P of id

 type connection_node =  
  | Connections of connection * connection_node list

 type circuit = wire * connection_node

let json_of_combinator (c: combinator) = 
  let id, name = begin match c with
 | Arithmetic (id, cfg) -> id, "arithmetic-combinator"
 | Decider (id, cfg) ->  id, "decider-combinator"
 | Constant (id, cfg) -> id, "constant-combinator"
 | Pole id -> id, "small-electric-pole"
  end in 
  `Assoc [("entity_number", `Int id); 
          ("name", `String name);
          ("position", `Assoc [("x", id * 2); ("y", 0)])
          ]

 let json_of_circuit (id: id) (circuit: circuit) = 
  `Assoc [ ]

let json_of_circuits (circuits: circuit list) = 
  let circs = List.mapi json_of_circuit circuits in 
  circs