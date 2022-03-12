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

%token LPAREN
%token RPAREN

%token <string> VAR
%token <int> LIT

%start toplevel

%type <Ast.bexp> toplevel  
%type <Ast.bexp> bexp
%%

toplevel:
  | b=bexp EOF { b }        

bexp:
  | b=b1 { b }  

b1:
  | l=b1 PLUS r=b2  { Plus(l, r) }
  | l=b1 MINUS r=b2 { Minus(l, r) }
  | b=b2            { b }

b2:
  | l=b2 MUL r=b3    { Mul(l, r) }
  | l=b2 DIV r=b3    { Div(l, r) }
  | l=b2 MOD r=b3    { Mod(l, r) }
  | l=b2 LSHIFT r=b3 { Lshift(l, r) }
  | l=b2 RSHIFT r=b3 { Rshift(l, r) }
  | l=b2 AND r=b3    { AND(l, r) }
  | l=b2 OR r=b3     { OR(l, r) }
  | l=b2 XOR r=b3    { XOR(l, r) }
  | b=b3             { b }

b3:
  | l=b3 EXP r=b4  { Exp(l, r) }
  | b=b4               { b }

b4: 
  | MINUS b=b4      { Neg(b) }
  | b=b5            { b }

b5: 
  | LPAREN b=bexp RPAREN { b }
  | b=b6            { b }

b6:
  | x=VAR   { Var x }
  | l=LIT   { Lit l }