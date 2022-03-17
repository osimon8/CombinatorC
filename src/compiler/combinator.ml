type symbol = string

type id = int

type value = int32

type data = symbol * value

type signal = data list

type size = int * int

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
 | Const of value
 | Each

 type dop = 
 | Symbol of string
 | Const of value
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

let size_of_combinator (comb:combinator) : size = 
 begin match comb with 
 | Arithmetic _  
 | Decider _ -> (1, 2)
 | Constant _ -> (1, 1)
 | Pole _ -> (2, 2) (* TODO: handle other size poles? *)
end

let id_of_combinator (comb:combinator) : id = 
  begin match comb with 
  | Arithmetic (id, _) -> id 
  | Decider (id, _) -> id
  | Constant (id, _) -> id
  | Pole id -> id
  end

(* let input_signals_of_combinator (comb:combinator) : op list = 
  begin match comb with 
  | Arithmetic (_, ((o1, _, o2, _))) -> [o1; o2]
  | _ -> []
  end *)

let uses_signal (comb:combinator) (s:symbol) : bool =
  let aop_uses (aop:aop) = 
    begin match aop with 
    | Symbol s1 -> s1 = s 
    | Const _ -> false 
    | Each -> true 
    end in

  let dop_uses dop inn t = 
    begin match dop with 
    | Symbol s1 -> (inn || (match t with | InpCount -> true | One -> false)) && s1 = s 
    | Const _ -> false
    | Anything
    | Everything 
    | Each -> true 
    end in 

  begin match comb with 
  | Arithmetic (id, (op1, _, op2, _)) -> aop_uses op1 || aop_uses op2
  | Decider (id, (op1, _, op2, op3, t)) -> dop_uses op1 true t || dop_uses op2 true t || dop_uses op3 false t
  | Constant (_, sigs) -> List.mem s (List.map fst sigs)  
  | Pole _ -> false 
  end 

let uses_signal_in_input (comb:combinator) (s:symbol) : bool = 
    let aop_uses (aop:aop) = 
    begin match aop with 
    | Symbol s1 -> s1 = s 
    | Const _ -> false 
    | Each -> true 
    end in

  let dop_uses dop = 
    begin match dop with 
    | Symbol s1 -> s1 = s 
    | Const _ -> false
    | Anything
    | Everything 
    | Each -> true 
    end in 

  begin match comb with 
  | Arithmetic (id, (op1, _, op2, _)) -> aop_uses op1 || aop_uses op2
  | Decider (id, (op1, _, op2, op3, t)) -> dop_uses op1 || dop_uses op2
  | Constant (_, sigs) -> List.mem s (List.map fst sigs)  
  | Pole _ -> false 
  end 


let uses_wildcard (comb:combinator) : bool = 
    let aop_uses (aop:aop) = 
    begin match aop with 
    | Each -> true 
    | _ -> false
    end in

  let dop_uses dop = 
    begin match dop with 
    | Anything
    | Everything 
    | Each -> true 
    | _ -> false
    end in 

  begin match comb with 
  | Arithmetic (id, (op1, _, op2, op3)) -> aop_uses op1 || aop_uses op2 || aop_uses op3
  | Decider (id, (op1, _, op2, op3, t)) -> dop_uses op1 || dop_uses op2  || dop_uses op3 
  | Constant (_, sigs) -> false
  | Pole _ -> false 
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

let string_of_combinator (comb:combinator) : string = 
  begin match comb with 
  | Arithmetic (id, _) -> "Arithmetic: " ^ string_of_int id
  | Decider (id, _) -> "Decider: " ^ string_of_int id
  | Constant (id, _) -> "Constant: " ^ string_of_int id
  | Pole id -> "Pole: " ^ string_of_int id
end