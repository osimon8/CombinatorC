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
  | l=b5 PLUS r=b1 { Plus(l, r) }
  | l=b5 MINUS r=b1 { Minus(l, r) }
  | l=b5 MUL r=b1 { Mul(l, r) }
  | l=b5 DIV r=b1 { Div(l, r) }
  | b=b5{ b }

b5:
  | x=VAR   { Var x }
  | l=LIT   { Lit l }