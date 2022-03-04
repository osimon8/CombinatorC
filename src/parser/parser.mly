%{
open Ast;;
%}

%token EOF
%token PLUS
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
  | b=b5{ b }

b5:
  | x=VAR   { Var x }
  | l=LIT   { Lit l }