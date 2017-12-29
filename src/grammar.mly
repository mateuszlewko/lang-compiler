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
%token LPAR RPAR EOF
%token EQ
%token INDENT DEDENT NEWLINE

%start <Ast.expr> top_let

%%

top_let:
  | NEWLINE; e = top_let { e }
  /* | e = top_let; NEWLINE { e } */
  | e = letexp; EOF { e }
  ;

letexp: 
  | LET; n = SYMBOL; vs = list(SYMBOL); EQ; e = simple_expr; NEWLINE
    { LetExp (n, vs, Some e, None) }  
  ;

literal:
  | i = INT { Int i }
  ;

simple_expr: 
  | l = literal { LitExp l }
  | s = SYMBOL { VarExp s }
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
