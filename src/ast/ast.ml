type bexp =
  | Var of string
  | Lit of int
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
  | Gt of bexp * bexp
  | Lt of bexp * bexp
  | Gte of bexp * bexp
  | Lte of bexp * bexp
  | Eq of bexp * bexp
  | Neq of bexp * bexp
  | Neg of bexp
  | Not of bexp
  | LAND of bexp * bexp
  | LOR of bexp * bexp
  | NAND of bexp * bexp
  | NOR of bexp * bexp
  | BOOL of bexp

type assignment = string * bexp

let vars_in_bexp (b:bexp) : string list = 
  let rec intern b =  
    begin match b with 
    | Var v -> [ v ]
    | Lit _ -> []
    | Not b 
    | Neg b 
    | BOOL b -> intern b
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
    | Div (b1, b2)
    | Gt (b1, b2)
    | Lt (b1, b2)
    | Gte (b1, b2)
    | Lte (b1, b2)
    | Eq (b1, b2)
    | Neq (b1, b2)
    | LAND (b1, b2)
    | LOR (b1, b2)
    | NAND (b1, b2)
    | NOR (b1, b2) -> intern b1 @ intern b2
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

    | Mul (_, Lit 0)
    | Mul (Lit 0, _)
    | AND (_, Lit 0)
    | AND (Lit 0, _) -> Lit 0

    | Exp (_, Lit 0) -> Lit 1

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

    (* BEGIN SECTION N - NECESSARY FOR PROPER COMPILATION *)
    | Gt (Lit l1, Lit l2) -> if l1 > l2 then Lit 1 else Lit 0
    | Lt (Lit l1, Lit l2) -> if l1 < l2 then Lit 1 else Lit 0
    | Gte (Lit l1, Lit l2) -> if l1 >= l2 then Lit 1 else Lit 0
    | Lte (Lit l1, Lit l2) -> if l1 <= l2 then Lit 1 else Lit 0
    | Eq (Lit l1, Lit l2) -> if l1 == l2 then Lit 1 else Lit 0
    | Neq (Lit l1, Lit l2) -> if l1 <> l2 then Lit 1 else Lit 0
    
    | Gt (Lit l1, b) -> Lte (o b, Lit l1)
    | Lt (Lit l1, b) -> Gte (o b, Lit l1)
    | Gte (Lit l1, b) -> Lt (o b, Lit l1)
    | Lte (Lit l1, b) -> Gt (o b, Lit l1)
    | Eq (Lit l1, b) -> Eq (o b, Lit l1)
    | Neq (Lit l1, b) -> Neq (o b, Lit l1)

    | LAND (Lit l1, Lit l2) -> if l1 <> 0 && l2 <> 0 then Lit 1 else Lit 0
    | LOR (Lit l1, Lit l2) -> if l1 <> 0 || l2 <> 0 then Lit 1 else Lit 0
    (* END SECTION N*)

    | Not Gt (b1, b2) -> Lte (o b1, o b2)
    | Not Lt (b1, b2) -> Gte (o b1, o b2)
    | Not Gte (b1, b2) -> Lt (o b1, o b2)
    | Not Lte (b1, b2) -> Gt (o b1, o b2)
    | Not Eq (b1, b2) -> Neq (o b1, o b2)
    | Not Neq (b1, b2) -> Eq (o b1, o b2)

    | Not Lit l -> if l = 0 then Lit 1 else Lit 0 
    | Not Not Not b -> Not (o b)
    | Not Not b -> BOOL (o b)

    (* Nots and BOOLS can be optimized away, prefer them *)
    | Eq (b, Lit 0) -> Not (o b)
    | Eq (b, Lit 1) -> BOOL (o b)

    (* LAND ands LORS take 2 combinators each, minimize their usage when possible *)
    | LAND (LAND (b1, b2), b3) -> LAND (Mul (o b1, o b2), o b3) 
    | LOR (LAND (b1, b2), b3) -> LOR (Mul (o b1, o b2), o b3) 
    | NAND (LAND (b1, b2), b3) -> NAND (Mul (o b1, o b2), o b3) 
    | NOR (LAND (b1, b2), b3) -> NOR (Mul (o b1, o b2), o b3) 
    | Not LAND (b1, b2) -> NAND (o b1, o b2) 
    | Not LOR (b1, b2) -> NOR (o b1, o b2) 
    | Not NAND (b1, b2) -> LAND (o b1, o b2) 
    | Not NOR (b1, b2) -> LOR (o b1, o b2) 
    (* demorgan *)
    | LAND (Not b1, Not b2) -> NOR (o b1, o b2) 
    | LOR (Not b1, Not b2) -> NAND (o b1, o b2) 

    | LOR (b, Lit l) 
    | LOR (Lit l, b) -> if l <> 0 then Lit 1 else BOOL (o b)

    | LAND (b, Lit l) 
    | LAND (Lit l, b) -> if l = 0 then Lit 0 else BOOL (o b)

    (* advanced demorgan *)
    | NOR (b, Lit l) 
    | NOR (Lit l, b) -> if l <> 0 then Lit 0 else Not (o b)

    | NAND (b, Lit l) 
    | NAND (Lit l, b) -> if l = 0 then Lit 1 else Not (o b)

    | LAND (BOOL b1, b2)
    | LAND (b2, BOOL b1) -> LAND (o b1, o b2)

    | LOR (BOOL b1, b2)
    | LOR (b2, BOOL b1) -> LOR (o b1, o b2)

    | NAND (BOOL b1, b2)
    | NAND (b2, BOOL b1) -> NAND (o b1, o b2)

    | NOR (BOOL b1, b2)
    | NOR (b2, BOOL b1) -> NOR (o b1, o b2)

    | BOOL (LAND (b1, b2)) -> LAND (b1, b2)
    | BOOL (LOR (b1, b2)) -> LOR (b1, b2)
    | BOOL (NAND (b1, b2)) -> NAND (b1, b2)
    | BOOL (NOR (b1, b2)) -> NOR (b1, b2)

    | BOOL Not b 
    | Not BOOL b -> Not b
    
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
    | Gt (b1, b2) -> Gt (o b1, o b2)
    | Lt (b1, b2) -> Lt (o b1, o b2)
    | Gte (b1, b2) -> Gte (o b1, o b2)
    | Lte (b1, b2) -> Lte (o b1, o b2)
    | Eq (b1, b2) -> Eq (o b1, o b2)
    | Neq (b1, b2) -> Neq (o b1, o b2)
    | LAND (b1, b2) -> LAND (o b1, o b2)
    | LOR (b1, b2) -> LOR (o b1, o b2)
    | NAND (b1, b2) -> NAND (o b1, o b2)
    | NOR (b1, b2) -> NOR (o b1, o b2)
    | Not b -> Not (o b)
    | BOOL b -> BOOL (o b)
    | Lit _
    | Var _ -> b
    end in 

  (* do the optimization n times *)
  let rec opti b i = if i = 0 then b else let b1 = o b in opti b1 (i - 1)  in
  opti b passes

let string_of_bexp (b : bexp) : string =
  let rec sobi first b =
    let sob = sobi false in
    let bin b1 b2 op = let s = sob b1 ^ " " ^ op ^ " " ^ sob b2 in 
                        if first then s else "(" ^ s ^ ")" in
    begin
        match b with
        | Var s -> s
        | Lit l -> string_of_int l
        | Neg b -> "-" ^ sob b
        | Not b -> "!" ^ sob b 
        | BOOL b -> "(bool) " ^ sob b
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
        | Gt (b1, b2) -> bin b1 b2 ">"
        | Lt (b1, b2) -> bin b1 b2 "<"
        | Gte (b1, b2) -> bin b1 b2 ">="
        | Lte (b1, b2) -> bin b1 b2 "<="
        | Eq (b1, b2) -> bin b1 b2 "=="
        | Neq (b1, b2) -> bin b1 b2 "!="
        | LAND (b1, b2) -> bin b1 b2 "&&"
        | LOR (b1, b2) -> bin b1 b2 "||"
        | NAND (b1, b2) -> sobi first (Not (LAND (b1, b2))) 
        | NOR (b1, b2) -> sobi first (Not (LOR (b1, b2))) 
      end
  in
  sobi true b