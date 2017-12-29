%{
open Core
open Ast
%}

%token <string> SYMBOL
%token <int> INT 
%token <bool> BOOL 
%token <string> STRING 
%token <char> KWD
%token LET IF ELSE ELIF THEN MODULE
%token LPAR RPAR
%token EQ QUOTE COMMA COLON SEMICOL 
%token INDENT DEDENT NEWLINE EOF

%start <Ast.expr> top_let
%start <Ast.program> program

%%

program: 
  option(NEWLINE); es = list(complex_expr); option(NEWLINE); EOF { Prog (es) }

top_let:
  | NEWLINE; e = top_let { e }
  | e = letexp; option(NEWLINE) { e }

letexp: 
  | LET; n = SYMBOL; vs = list(SYMBOL); EQ; e = option(simple_expr); NEWLINE;
    es = option(indented)
    { LetExp (n, vs, e, es) } 

application:
  | s = simple_expr; es1 = nonempty_list(simple_expr); option(NEWLINE); 
    es2 = option(indented); option(NEWLINE)
    { AppExp (s, es1, es2) }
  | s = simple_expr; NEWLINE; 
    es2 = indented; option(NEWLINE)
    { AppExp (s, [], Some es2) }

indented:
  INDENT; es = list(complex_expr); DEDENT { es }

literal:
  | i = INT { Int i } 
  | b = BOOL { Bool b }

simple_expr: 
  | l = literal { LitExp l }
  | LPAR; a = application; RPAR { a }
  | s = SYMBOL { VarExp s }
  | LPAR; e = simple_expr; RPAR { e }

complex_expr:
  | l = letexp; list(NEWLINE) { l }
  | s = simple_expr; list(NEWLINE) { s }
  | a = application; list(NEWLINE) { a }
  | LPAR; e = complex_expr; RPAR; list(NEWLINE) { e }

%%
