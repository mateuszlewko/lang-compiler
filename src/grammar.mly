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
  /* | e = letexp; { e } */
  | e = letexp; option(NEWLINE) { e }
  /* | e = letexp; newlines { e } */
  /* | NEWLINE; e = letexp; NEWLINE { e } */
  /* | NEWLINE; e = top_let; NEWLINE { e } */
  /* | e = top_let; NEWLINE { e } */
  /* | e = top_let; EOF { e } */
  /* | e = top_let; NEWLINE { e } */
  ;

letexp: 
  /* | LET; n = SYMBOL; vs = list(SYMBOL); EQ; e1 = simple_expr; NEWLINE; 
    INDENT; e2 = complex_expr; DEDENT 
    { LetExp (n, vs, Some e1, Some [e2]) }  */
  | LET; n = SYMBOL; vs = list(SYMBOL); EQ; e = option(simple_expr); NEWLINE;
    es = option(indented)
    { LetExp (n, vs, e, es) } 
  ;

indented:
  | INDENT; es = list(complex_expr); DEDENT { es }

literal:
  | i = INT { Int i } 
  ;

simple_expr: 
  | l = literal { LitExp l }
  | s = SYMBOL { VarExp s }
  | LPAR; e = simple_expr; RPAR { e }
  ;

complex_expr:
  | l = letexp; NEWLINE { l }
  | s = simple_expr; NEWLINE { s }
  | LPAR; e = complex_expr; RPAR { e }
  ;

/* ast:
  | (* empty *)  { "" }
  | s = SYMBOL; a = ast { sprintf "sym %s; %s" s a }
  | s = KWD; a = ast { sprintf "kwd '%c'; %s" s a }
  | LET; a = ast { sprintf "let; %s" a }
  | INDENT; a = ast { sprintf "indent; %s" a }
  | DEDENT; a = ast { sprintf "dedent; %s" a }
  | NEWLINE; a = ast { sprintf "newline; %s" a }
  | l = list(nested) { String.concat l } */

/* nested:
  | LPAR; a = ast; RPAR { sprintf "(%s)" a }
  ; */

%%
