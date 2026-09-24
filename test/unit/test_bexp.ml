open Ast.Bexp

let failures = ref 0

let check name (b : bexp) (expected : int32) =
  match interpret_bexp b with
  | Some v when v = expected -> ()
  | Some v ->
      incr failures;
      Printf.printf "FAIL %s: %s evaluated to %ld, expected %ld\n" name (string_of_bexp b) v
        expected
  | None ->
      incr failures;
      Printf.printf "FAIL %s: %s could not be evaluated\n" name (string_of_bexp b)
  | exception e ->
      incr failures;
      Printf.printf "FAIL %s: %s raised %s\n" name (string_of_bexp b) (Printexc.to_string e)

let l i = Lit (Int32.of_int i)

let () =
  (* Basic arithmetic *)
  check "plus" (Plus (l 2, l 3)) 5l;
  check "minus" (Minus (l 2, l 3)) (-1l);
  check "mul" (Mul (l 4, l 3)) 12l;
  check "div truncates" (Div (l 7, l 2)) 3l;
  check "mod" (Mod (l 7, l 3)) 1l;
  check "nested" (Plus (Mul (l 2, l 3), Div (l 10, l 5))) 8l;

  (* Division by zero yields 0 in Factorio *)
  check "div by zero" (Div (l 5, l 0)) 0l;
  check "mod by zero" (Mod (l 5, l 0)) 0l;
  check "div by zero nested" (Plus (l 1, Div (l 5, l 0))) 1l;

  (* Exponentiation *)
  check "pow" (Exp (l 2, l 10)) 1024l;
  check "pow zero" (Exp (l 7, l 0)) 1l;
  check "pow one" (Exp (l 7, l 1)) 7l;
  check "pow negative base" (Exp (l (-3), l 3)) (-27l);
  check "pow negative exponent" (Exp (l 2, l (-1))) 0l;
  check "pow min exponent" (Exp (l 2, Lit Int32.min_int)) 0l;
  check "pow large exponent" (Exp (l 1, Lit Int32.max_int)) 1l;
  check "pow wraps" (Exp (l 2, l 32)) 0l;
  check "pow wraps 31" (Exp (l 2, l 31)) Int32.min_int;

  (* Shifts only use the low 5 bits of the shift amount *)
  check "lshift" (Lshift (l 1, l 4)) 16l;
  check "rshift" (Rshift (l 256, l 4)) 16l;
  check "rshift arithmetic" (Rshift (l (-16), l 2)) (-4l);
  check "lshift 32" (Lshift (l 1, l 32)) 1l;
  check "lshift 33" (Lshift (l 1, l 33)) 2l;
  check "lshift negative" (Lshift (l 1, l (-1))) Int32.min_int;

  (* Bitwise *)
  check "and" (AND (l 12, l 10)) 8l;
  check "or" (OR (l 12, l 10)) 14l;
  check "xor" (XOR (l 12, l 10)) 6l;

  (* Comparisons *)
  check "eq true" (Eq (l 5, l 5)) 1l;
  check "eq false" (Eq (l 5, l 6)) 0l;
  check "eq large" (Eq (l 100000, l 100000)) 1l;
  check "neq" (Neq (l 5, l 6)) 1l;
  check "gt" (Gt (l 5, l 6)) 0l;
  check "lt" (Lt (l 5, l 6)) 1l;
  check "gte" (Gte (l 6, l 6)) 1l;
  check "lte" (Lte (l 7, l 6)) 0l;

  (* Logic *)
  check "land" (LAND (l 3, l 0)) 0l;
  check "lor" (LOR (l 3, l 0)) 1l;
  check "not" (Not (l 3)) 0l;
  check "not not" (Not (Not (l 3))) 1l;
  check "not not zero" (Not (Not (l 0))) 0l;
  check "bool" (BOOL (l (-7))) 1l;
  check "conditional true" (Conditional (l 1, l 10, l 20)) 10l;
  check "conditional false" (Conditional (l 0, l 10, l 20)) 20l;
  check "conditional eq" (Conditional (Eq (l 5, l 5), l 10, l 20)) 10l;

  if !failures > 0 then (
    Printf.printf "%d test(s) failed\n" !failures;
    exit 1)
