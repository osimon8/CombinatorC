type symbol = string

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

type op = Aop of aop | Dop of dop

(* left input * operation * right input * output  *)
type arithemtic_config = aop * arithemtic_op * aop * aop 

(* left input * operation * right input * output  *)
type decider_config = dop * decider_op * dop * dop * decider_output_type

type constant_config = signal

type cfg = 
| A of arithemtic_config 
| D of decider_config 
| C of constant_config

type arithmetic_combinator = id * arithemtic_config
type decider_combinator = id * decider_config

type combinator = 
 | Arithmetic of arithmetic_combinator
 | Decider of decider_combinator
 | Constant of id * constant_config
 | Pole of id

let id_of_combinator comb = 
  begin match comb with 
  | Arithmetic (id, _) -> id 
  | Decider (id, _) -> id
  | Constant (id, _) -> id
  | Pole id -> id
  end

let replace_signal_A (comb:arithmetic_combinator) (s:symbol) (v:value) : arithmetic_combinator =
  let r2 (comb:arithmetic_combinator) s v : arithmetic_combinator =
    let id, ((o1, op, o2, out)) = comb in 
    begin match o2 with 
    | Symbol sy -> if sy = s then (id, (o1, op, Const v, out)) else comb
    | _ -> comb 
    end
  in

   let id, ((o1, op, o2, out)) = comb in 
   begin match o1 with 
   | Symbol sy -> if sy = s then r2 (id, (Const v, op, o2, out)) s v else r2 comb s v
   | _ -> r2 comb s v
end

let replace_signal_D (comb:decider_combinator) (s:symbol) (v:value) : decider_combinator =
  let r2 (comb:decider_combinator) s v : decider_combinator =
    let id, ((o1, op, o2, out, t)) = comb in 
    begin match o2 with 
    | Symbol sy -> if sy = s then (id, (o1, op, Const v, out, t)) else comb
    | _ -> comb 
    end
  in

   let id, ((o1, op, o2, out, t)) = comb in 
   begin match o1 with 
   | Symbol sy -> if sy = s then r2 (id, (Const v, op, o2, out, t)) s v else r2 comb s v
   | _ -> r2 comb s v
end

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
 | AND -> "AND"
 | OR -> "OR"
 | XOR -> "XOR"
end

let string_of_decider_op (op:decider_op) : string = 
 begin match op with 
 | Gt -> ">"
 | Lt -> "<"
 | Gte -> "≥"
 | Lte -> "≤"
 | Eq -> "="
 | Neq -> "≠"
end