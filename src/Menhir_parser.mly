%{
open Core

%}

%token <string> SYMBOL
%token <int> INT 
%token <bool> BOOL 
%token <string> STRING 
%token <char> KWD
%token LET IF ELSE ELIF THEN MODULE
%token LPAR RPAR EOF
%token INDENT DEDENT NEWLINE

%start <string> ast_eof

%%

ast_eof:
  | a = ast; EOF { a }
  ;

ast:
  | (* empty *)  { "" }
  | s = SYMBOL; a = ast { sprintf "sym %s; %s" s a }
  | s = KWD; a = ast { sprintf "kwd '%c'; %s" s a }
  | LET; a = ast { sprintf "let; %s" a }
  | INDENT; a = ast { sprintf "indent; %s" a }
  | DEDENT; a = ast { sprintf "dedent; %s" a }
  | NEWLINE; a = ast { sprintf "newline; %s" a }
  /* | l = list(nested) { String.concat l } */

/* nested:
  | LPAR; a = ast; RPAR { sprintf "(%s)" a }
  ; */

%%
