type bexp =
  | Var of string
  | Lit of int
  | Neg of bexp
  | Plus of bexp * bexp
  | Minus of bexp * bexp
  | Mul of bexp * bexp
  | Div of bexp * bexp


let vars_in_bexp (b:bexp) : string list = 
  let rec intern b =  
    begin match b with 
    | Var v -> [ v ]
    | Lit _ -> []
    | Neg b -> intern b
    | Plus (b1, b2)
    | Minus (b1, b2)
    | Mul (b1, b2) 
    | Div (b1, b2) -> intern b1 @ intern b2
    end in
  (Core.List.stable_dedup (intern b))

let string_of_bexp (b : bexp) : string =
  (* b is the bexp, l is the outer precedence level *)
  let rec sob b =
    begin
        match b with
        | Var s -> s
        | Lit l -> string_of_int l
        | Neg b -> "-" ^ sob b
        | Plus (b1, b2) -> sob b1 ^ "+" ^ sob b2
        | Minus (b1, b2) -> sob b1 ^ "-" ^ sob b2
        | Mul (b1, b2) -> sob b1 ^ "*" ^ sob b2
        | Div (b1, b2) -> sob b1 ^ "/" ^ sob b2
      end
  in
  sob b