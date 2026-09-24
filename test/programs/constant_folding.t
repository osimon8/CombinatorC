Constant expressions are folded at compile time using Factorio's arithmetic
semantics. Only the combinator settings are shown here.

  $ show () { combc --output-json "$1" 2>/dev/null | grep -E '"(first|second|output)_(signal|constant)"|"(operation|comparator)"'; }

Division and modulo by zero evaluate to 0, so this is just B.

  $ cat > div_zero.combc << EOF
  > circuit c : A = B + 5 / 0 + 7 % 0;
  > output c;
  > EOF
  $ show div_zero.combc
              "first_signal": { "type": "virtual", "name": "signal-B" },
              "second_constant": 0,
              "operation": "+",
              "output_signal": { "type": "virtual", "name": "signal-A" }

A negative exponent evaluates to 0.

  $ cat > neg_pow.combc << EOF
  > circuit c : A = B + 2 ** -1;
  > output c;
  > EOF
  $ show neg_pow.combc
              "first_signal": { "type": "virtual", "name": "signal-B" },
              "second_constant": 0,
              "operation": "+",
              "output_signal": { "type": "virtual", "name": "signal-A" }

Large exponents fold quickly and wrap around like Factorio's 32 bit integers.

  $ cat > big_pow.combc << EOF
  > circuit c : A = B * 3 ** 1000000000;
  > output c;
  > EOF
  $ show big_pow.combc
              "first_signal": { "type": "virtual", "name": "signal-B" },
              "second_constant": 783845377,
              "operation": "*",
              "output_signal": { "type": "virtual", "name": "signal-A" }

Literal equality holds, so the true branch (B) is selected.

  $ cat > eq.combc << EOF
  > circuit c : A = if 5 == 5 then B else C;
  > output c;
  > EOF
  $ show eq.combc
              "first_signal": { "type": "virtual", "name": "signal-B" },
              "second_constant": 0,
              "operation": "+",
              "output_signal": { "type": "virtual", "name": "signal-A" }

Double negation of a literal folds to its truth value.

  $ cat > not_not.combc << EOF
  > int x = !!5;
  > circuit c : A = B + x;
  > output c;
  > EOF
  $ show not_not.combc
              "first_signal": { "type": "virtual", "name": "signal-B" },
              "second_constant": 1,
              "operation": "+",
              "output_signal": { "type": "virtual", "name": "signal-A" }

Shift amounts only use their low 5 bits.

  $ cat > shift.combc << EOF
  > circuit c : A = B + (1 << 33);
  > output c;
  > EOF
  $ show shift.combc
              "first_signal": { "type": "virtual", "name": "signal-B" },
              "second_constant": 2,
              "operation": "+",
              "output_signal": { "type": "virtual", "name": "signal-A" }
