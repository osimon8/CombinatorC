%{
open Ast.Bexp;;
open Ast.Command;;
open Ast.Expression;;
open Compiler.Directive;;
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
%token LEQ
%token LNEQ

%token LPAREN
%token RPAREN

%token CONCRETE
%token CIRCUIT_BIND
%token ASSIGN
%token SEMI

%token IF 
%token THEN 
%token ELSE

// %token QUESTION 
%token COLON
%token COALESCE

%token <string * string> DIRECTIVE
%token <string> IDENT

%token UNION
%token CONCAT

%token OUTPUT
%token AT 
%token COMMA

%token <string> VAR
%token <int32> LIT

%nonassoc ELSE

%left COALESCE
%left LOR 
%left LAND
%left OR
%left XOR 
%left AND 
%left EQ NEQ LEQ LNEQ 
%left LT GT LTE GTE 
%left LSHIFT RSHIFT 
%left PLUS MINUS 
%left MUL DIV MOD 
%right EXP  


%nonassoc NOT 

%left UNION 
%left CONCAT 

%start toplevel

%on_error_reduce program

%type <directive list * command list> toplevel  
%type <bexp> bexp
%type <expression> expression
%%

toplevel:
  | p=program EOF { p }

program:
  | d=dir_seq c=c_seq  { (d, c) }
  | c=c_seq             { ([], c) }

c_seq:
  | l=list(command) { l }

dir_seq: 
  | d1=dir_seq d2=directive   { d1 @ d2 }
  | d=directive               { d }

directive:
  | d=DIRECTIVE   { [parse_directive (fst d) (snd d)] } 

command:
  | CONCRETE CIRCUIT_BIND i=IDENT COLON v=b_var ASSIGN b=bexp SEMI { CircuitBind (i, b, v, true) }
  | CIRCUIT_BIND i=IDENT COLON v=b_var ASSIGN b=bexp SEMI          { CircuitBind (i, b, v, false) }
  | CIRCUIT_BIND i=IDENT ASSIGN e=expression SEMI                  { Assign (i, TCircuit, e) }
  | o=output SEMI                                                  { o }

output:
  | OUTPUT c=circuit AT LPAREN v1=LIT COMMA v2=LIT RPAREN      { let loc = (Int32.to_float v1, Int32.to_float v2) in OutputAt ((Circuit c), loc) }
  | OUTPUT c=circuit                                           { Output (Circuit c) }
  | OUTPUT b=bexp AT LPAREN v1=LIT COMMA v2=LIT RPAREN      { let loc = (Int32.to_float v1, Int32.to_float v2) in OutputAt (expression_of_bexp b, loc) }
  | OUTPUT b=bexp                                           { Output (expression_of_bexp b) }

circuit:
  | c1=circuit UNION c2=circuit    { Union (c1, c2, None) }
  | c1=circuit CONCAT c2=circuit   { Concat (c1, c2, None) }
  | c=expression                   { Expression (c, None) }
  | LPAREN c=circuit RPAREN          { c }

arg: 
  | e=expression                 { e }
  | b=bexp                       { expression_of_bexp b }

expression: 
  // | b=bexp    { expression_of_bexp b }
  | c=call    { c }
  | i=IDENT   { Ast.Expression.Var i }
  // | LPAREN e=expression RPAREN     { e }

%inline call: 
  | p=IDENT LPAREN args=separated_list(COMMA, arg) RPAREN { Call (p, args) }

bexp:
  | IF g=bexp THEN b1=bexp ELSE b2=bexp     { Conditional(g, b1, b2) }
  | b=b_main                                { b }

b_main:
  | MINUS b=bexp                          { Neg(b) }
  | NOT b=bexp                            { Not(b) }
  | b=bop                                 { b }
  | l=LIT                                 { Lit l }
  | x=VAR                                 { Var x }
  | LPAREN b=bexp RPAREN                  { b }

%inline bop:
  | b1=bexp LOR b2=bexp              { LOR(b1, b2) }
  | b1=bexp LAND b2=bexp             { LAND(b1, b2) }
  | b1=bexp COALESCE b2=bexp         { Conditional(b1, b1, b2) }
  | b1=bexp OR b2=bexp               { OR(b1, b2) }
  | b1=bexp XOR b2=bexp              { XOR(b1, b2) }
  | b1=bexp AND b2=bexp              { AND(b1, b2) }
  | b1=bexp EQ b2=bexp               { Eq(b1, b2) }
  | b1=bexp NEQ b2=bexp              { Neq(b1, b2) }
  | b1=bexp LEQ b2=bexp              { Not(XOR (BOOL b1, BOOL b2)) }
  | b1=bexp LNEQ b2=bexp             { XOR((BOOL b1, BOOL b2)) }
  | b1=bexp LT b2=bexp               { Lt(b1, b2) }
  | b1=bexp GT b2=bexp               { Gt(b1, b2) }
  | b1=bexp LTE b2=bexp              { Lte(b1, b2) } 
  | b1=bexp GTE b2=bexp              { Gte(b1, b2) }
  | b1=bexp LSHIFT b2=bexp            { Lshift(b1, b2) }
  | b1=bexp RSHIFT b2=bexp           { Rshift(b1, b2) }
  | b1=bexp PLUS b2=bexp             { Plus(b1, b2) }
  | b1=bexp MINUS b2=bexp            { Minus(b1, b2) }
  | b1=bexp MUL b2=bexp              { Mul(b1, b2) } 
  | b1=bexp DIV b2=bexp              { Div(b1, b2) }
  | b1=bexp MOD b2=bexp              { Mod(b1, b2) } 
  | b1=bexp EXP b2=bexp              { Exp(b1, b2) } 

%inline b_var:
  | x=VAR   { x } 