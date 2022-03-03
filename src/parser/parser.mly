%{
open Ast;;
%}

%token EOF
%token <Range.t> PLUS      /* + */
%token <Range.t> LPAREN   /* ( */
%token <Range.t> RPAREN   /* ) */

/* ---------------------------------------------------------------------- */

/* Mark 'toplevel' as a starting nonterminal of the grammar */
%start toplevel           

/* Define type annotations for toplevel and bexp */
%type <Ast.bexp> toplevel  
%type <Ast.bexp> bexp
%%

toplevel:
  | b=bexp EOF { b }        

bexp:
  | b=b1 { b }  

b1:
  | l=b2 ARR r=b1 { Imp(l, r) }
  | b=b2 { b }

b2:
  | l=b2 BAR r=b3 { Or(l, r) }
  | b=b3 { b }

b3:
  | l=b3 AMPER r=b4 { And(l, r) }
  | b=b4 { b }

b4:
  | TILDE b=b4 { Not(b) }
  | b=b5 { b }

b5:
  | TRUE  { True }
  | FALSE { False }
  | x=VAR   { Var (snd x) }
  | LPAREN b=b1 RPAREN { b }
