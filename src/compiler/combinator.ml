type symbol = 
  | Red of string
  | Green of string

type id = int

type value = int

type data = symbol * value

type signal = data list

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

type decider_output_type = 
 | One
 | InpCount

type aop = 
 | Symbol of string
 | Const of int
 | Each

 type dop = 
 | Symbol of string
 | Const of int
 | Each
 | Anything
 | Everything

(* left input * operation * right input * output  *)
type arithemtic_config = aop * arithemtic_op * aop * aop 

(* left input * operation * right input * output  *)
type decider_config = dop * decider_op * dop * dop * decider_output_type

type constant_config = signal

type cfg = 
| A of arithemtic_config 
| D of decider_config 
| C of constant_config

type combinator = 
 | Arithmetic of id * arithemtic_config
 | Decider of id * decider_config
 | Constant of id * constant_config
 | Pole of id

let string_of_arithmetic_op (op:arithemtic_op) : string = 
  begin match op with 
 | Add -> "+"
 | Sub -> "-"
 | Mul -> "*"
 | Div -> "/"
 | Mod -> "%"
 | Exp -> "^"
 | Lshift -> "<<"
 | Rshift -> ">>" 
 | AND -> "&"
 | OR -> "|"
 | XOR -> "xor"
end