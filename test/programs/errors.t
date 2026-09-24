The compiler reports errors on stderr and exits with a non-zero status.

  $ cat > unknown.combc << EOF
  > circuit c : A = B + x;
  > output c;
  > EOF
  $ combc unknown.combc
  Unknown identifier: "x"
  [1]

  $ cat > duplicate.combc << EOF
  > int x = 1;
  > int x = 2;
  > EOF
  $ combc duplicate.combc
  Duplicate identifier: "x"
  [1]

  $ cat > syntax.combc << EOF
  > circuit c : A = B +;
  > EOF
  $ combc syntax.combc
  File "syntax.combc", line 1, characters 19-20:
  Syntax error after '+' and before ';'.
  Missing right operand of binary operator, syntax is "<exp> <bop> <exp>"
  [1]

  $ combc missing.combc
  File error
  missing.combc: No such file or directory
  [1]
