type bexp =
  | Var of string
  | Lit of int
  | Neg of bexp
  | Plus of bexp * bexp
  | Minus of bexp * bexp
  | Mul of bexp * bexp
  | Div of bexp * bexp
  | Mod of bexp * bexp
  | Exp of bexp * bexp
  | Lshift of bexp * bexp
  | Rshift of bexp * bexp
  | AND of bexp * bexp
  | OR of bexp * bexp
  | XOR of bexp * bexp


let vars_in_bexp (b:bexp) : string list = 
  let rec intern b =  
    begin match b with 
    | Var v -> [ v ]
    | Lit _ -> []
    | Neg b -> intern b
    | Plus (b1, b2)
    | Minus (b1, b2)
    | Mul (b1, b2) 
    | Mod (b1, b2) 
    | Exp (b1, b2) 
    | Lshift (b1, b2) 
    | Rshift (b1, b2) 
    | AND (b1, b2) 
    | OR (b1, b2) 
    | XOR (b1, b2) 
    | Div (b1, b2) -> intern b1 @ intern b2
    end in
  (Core.List.stable_dedup (intern b))


let optimize_bexp (b:bexp) : bexp = 
  let passes = 3 in 

  (* TODO: refactor bexp def so this isn't so disgusting *)
  let rec o b =  
    begin match b with 
    | Plus (Lit 0, b)
    | Plus (b, Lit 0)
    | Mul (Lit 1, b)
    | Mul (b, Lit 1)
    | Div (b, Lit 1)
    | Lshift (b, Lit 0)
    | Rshift (b, Lit 0)
    | OR (b, Lit 0)
    | OR (Lit 0, b)
    | XOR (b, Lit 0)
    | XOR (Lit 0, b)
    | Neg (Neg b) -> o b

    | Mul (b, Lit 0)
    | Mul (Lit 0, b)
    | AND (b, Lit 0)
    | AND (Lit 0, b) -> Lit 0

    | Exp (b, Lit 0) -> Lit 1

    | Mul (Lit -1, b)
    | Mul (b, Lit -1)
    | Div (b, Lit -1) -> Neg (o b)

    | Mul (b1, Exp (Lit 2, b2)) 
    | Mul (Exp (Lit 2, b2), b1) -> Lshift (o b1, o b2) 

    | Div (b1, Exp (Lit 2, b2)) -> Rshift (o b1, o b2) 

    | Plus (b, Lit l) -> if l < 0 then Minus (o b, Lit (l * -1)) else Plus (o b, Lit l)
    | Plus (b1, Neg b2) -> Minus (o b1, o b2)

    | Minus (b, Lit l) -> if l < 0 then Plus (o b, Lit (l * -1)) else Minus (o b, Lit l)
    | Minus (b1, Neg b2) -> Plus (o b1, o b2)

    | Mul (Neg b1, Neg b2) -> Mul (o b1, o b2)

    | Plus (b1, b2) -> Plus (o b1, o b2)
    | Minus (b1, b2) -> Minus (o b1, o b2)
    | Div (b1, b2) -> Div (o b1, o b2)
    | Mul (b1, b2) -> Mul (o b1, o b2)
    | Exp (b1, b2) -> Exp (o b1, o b2)
    | Mod (b1, b2) -> Mod (o b1, o b2)
    | Lshift (b1, b2) -> Lshift (o b1, o b2)
    | Rshift (b1, b2) -> Rshift (o b1, o b2)
    | AND (b1, b2) -> AND (o b1, o b2)
    | OR (b1, b2) -> OR (o b1, o b2)
    | XOR (b1, b2) -> XOR (o b1, o b2)
    | Neg b -> Neg (o b)
    | Lit _
    | Var _ -> b
    end in 

  (* do the optimization n times *)
  let rec opti b i = if i = 0 then b else let b1 = o b in opti b1 (i - 1)  in
  opti b passes

let string_of_bexp (b : bexp) : string =
  let rec sob b =
    let bin b1 b2 op = "(" ^ sob b1 ^ op ^ sob b2 ^ ")" in
    begin
        match b with
        | Var s -> s
        | Lit l -> string_of_int l
        | Neg b -> "-" ^ sob b
        | Plus (b1, b2) -> bin b1 b2 "+"
        | Minus (b1, b2) -> bin b1 b2 "-"
        | Mul (b1, b2) -> bin b1 b2 "*"
        | Div (b1, b2) -> bin b1 b2 "/"
        | Mod (b1, b2) -> bin b1 b2 "%"
        | Exp (b1, b2) -> bin b1 b2 "**"
        | Lshift (b1, b2) -> bin b1 b2 "<<"
        | Rshift (b1, b2) -> bin b1 b2 ">>"
        | AND (b1, b2) -> bin b1 b2 "&"
        | OR (b1, b2) -> bin b1 b2 "|"
        | XOR (b1, b2) -> bin b1 b2 "^"
      end
  in
  sob b