%{
open Ast;;
%}

%token EOF
%token PLUS
%token MINUS
%token MUL
%token DIV
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
| l=b2 MUL r=b3   { Mul(l, r) }
| l=b2 DIV r=b3   { Div(l, r) }
| b=b3            { b }

b3: 
| MINUS b=b4        { Neg(b) }
| b=b4            { b }

b4:
  | x=VAR   { Var x }
  | l=LIT   { Lit l }