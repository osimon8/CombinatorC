type bexp =
  | Var of string
  | Lit of int
  | Plus of bexp * bexp


let string_of_bexp (b : bexp) : string =
  (* b is the bexp, l is the outer precedence level *)
  let rec sob b =
    begin
        match b with
        | Var s -> s
        | Lit l -> string_of_int l
        | Plus (b1, b2) -> sob b1 ^ "+" ^ sob b2
      end
  in
  sob b