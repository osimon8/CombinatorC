%{
open Ast;;
%}

%token EOF
%token PLUS
%token MINUS
%token MUL
%token DIV
%token MOD
%token LSHIFT
%token RSHIFT
%token AND
%token OR
%token XOR
%token EXP

%token GT
%token LT
%token GTE
%token LTE
%token EQ
%token NEQ

%token NOT
%token LOR
%token LAND

%token LPAREN
%token RPAREN

%token CIRCUIT_BIND
%token ASSIGN
%token SEMI

%token <string> VAR
%token <int> LIT

%start toplevel

%type <Ast.assignment list> toplevel  
%type <Ast.bexp> bexp
%%

toplevel:
  | b=b_seq EOF { b }
  | b=bexp EOF { [("check", b)] }         

b_seq:
  | a1=b_seq SEMI a2=b_assn { a1 @ a2 }
  | b=b_assn    { b }

b_assn:
  | CIRCUIT_BIND v=b_var ASSIGN b=bexp { [(v, b)] }

bexp:
  | b=b_o { b } 

b_o:
  | l=b_o LOR r=b_a      { LOR(l, r) }
  | b=b_a               { b }

b_a:
  | l=b_a LAND r=b1     { LAND(l, r) }
  | b=b1               { b }

b1:
  | l=b1 OR r=b2       { OR(l, r) }
  | b=b2               { b }

b2:
  | l=b2 XOR r=b3      { XOR(l, r) }
  | b=b3               { b }

b3:
  | l=b3 AND r=b4      { AND(l, r) }
  | b=b4               { b }

b4:
  | l=b4 EQ r=b5       { Eq(l, r) }
  | l=b4 NEQ r=b5      { Neq(l, r) }
  | b=b5               { b }

b5:
  | l=b5 LT r=b6       { Lt(l, r) }
  | l=b5 GT r=b6       { Gt(l, r) }
  | l=b5 LTE r=b6      { Lte(l, r) }
  | l=b5 GTE r=b6      { Gte(l, r) }
  | b=b6               { b }

b6:
  | l=b6 LSHIFT r=b7   { Lshift(l, r) }
  | l=b6 RSHIFT r=b7   { Rshift(l, r) }
  | b=b7               { b }

b7:
  | l=b7 PLUS r=b8     { Plus(l, r) }
  | l=b7 MINUS r=b8    { Minus(l, r) }
  | b=b8               { b }

b8:
  | l=b8 MUL r=b9      { Mul(l, r) }
  | l=b8 DIV r=b9      { Div(l, r) }
  | l=b8 MOD r=b9      { Mod(l, r) }
  | b=b9               { b }

b9:
  | l=b9 EXP r=b10     { Exp(l, r) }
  | b=b10              { b }

b10: 
  | MINUS b=b10        { Neg(b) }
  | NOT b=b10          { Not(b) }
  | b=b11              { b }

b11: 
  | LPAREN b=bexp RPAREN  { b }
  | b=b12                 { b }

b12:
  | l=LIT   { Lit l }
  | x=VAR   { Var x }

b_var:
  | x=VAR   { x } 