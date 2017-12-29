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
%token INDENT DEDENT NEWLINE

%start <Ast.expr> top_let

%%

top_let:
  | NEWLINE; e = top_let { e }
  | e = letexp; option(NEWLINE) { e }

letexp: 
  | LET; n = SYMBOL; vs = list(SYMBOL); EQ; e = option(simple_expr); NEWLINE;
    es = option(indented)
    { LetExp (n, vs, e, es) } 

application:
  | s = simple_expr; es1 = nonempty_list(simple_expr); NEWLINE; es2 = option(indented)
    { AppExp (s, es1, es2) }

indented:
  | INDENT; es = list(complex_expr); DEDENT { es }

literal:
  | i = INT { Int i } 
  | b = BOOL { Bool b }

simple_expr: 
  | l = literal { LitExp l }
  | s = SYMBOL { VarExp s }
  | LPAR; e = simple_expr; RPAR { e }

complex_expr:
  | l = letexp; NEWLINE { l }
  | a = application { a }
  | s = simple_expr; NEWLINE { s }
  | LPAR; e = complex_expr; RPAR { e }

%%
