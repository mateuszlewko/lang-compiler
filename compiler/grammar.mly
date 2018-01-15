%{
open Core
open Ast
%}

%token <string> SYMBOL
%token <int> INT
%token <bool> BOOL
%token <string> STRING
%token <string> OPERATOR
%token <char> KWD
%token LET REC IF ELSE ELIF THEN MODULE TYPE
%token PIPE FUNCTION MATCH WITH ARROW
%token LPAR RPAR LBRACKET RBRACKET LCURLY RCURLY
%token EQ QUOTE COMMA COLON SEMICOL
%token INDENT DEDENT NEWLINE EOF
%left OPERATOR

%start <Ast.expr> top_let
%start <Ast.program> program

%%

program:
  option(NEWLINE); es = list(complex_expr); option(NEWLINE); EOF { Prog (es) }

type_anot:
  | { None }
  | COLON; t = SYMBOL { Some t }

typed_var:
  | LPAR; s = SYMBOL; COLON; t = SYMBOL; RPAR { s, Some t }
  | s = SYMBOL; t = type_anot { s, t }
  | LPAR; t = typed_var; RPAR { t }

top_let:
  | NEWLINE; e = top_let { e }
  | e = letexp; option(NEWLINE) { e }

letexp:
  | LET; n = SYMBOL; vs = list(typed_var); rett = type_anot;
    EQ; e = option(simple_expr); NEWLINE;
    es = option(indented)
    { LetExp ((n, rett), vs, e, es) }

application:
  | s = simple_expr; es1 = nonempty_list(simple_expr); option(NEWLINE);
    es2 = option(indented); option(NEWLINE)
    { AppExp (s, es1, es2) }
  | s = simple_expr; NEWLINE;
    es2 = indented; option(NEWLINE)
    { AppExp (s, [], Some es2) }

infix_op:
  | l = simple_expr; o = OPERATOR; r = simple_expr
    { InfixOp (o, Some l, Some r) }
  | l = simple_expr; EQ; r = simple_expr
    { InfixOp ("=", Some l, Some r) }

else_exp:
  | ELSE; exp = simple_expr {exp}

if_exp:
  | IF; cond = simple_expr; option(NEWLINE); THEN; true_ex = simple_expr;
    option(NEWLINE); else_ex = option(else_exp); option(NEWLINE)
    { IfExp (cond, true_ex, else_ex) }

indented:
  INDENT; es = list(complex_expr); DEDENT { es }

literal:
  | i = INT { Int i }
  | b = BOOL { Bool b }

simple_expr:
  | l = literal { LitExp l }
  | i = infix_op { i }
  | LPAR; a = application; RPAR { a }
  | s = SYMBOL { VarExp s }
  | LPAR; e = simple_expr; RPAR { e }

complex_expr:
  | l = letexp; list(NEWLINE) { l }
  | e = if_exp; list(NEWLINE) { e }
  | s = simple_expr; nonempty_list(NEWLINE) { s }
  | a = application; list(NEWLINE) { a }
  | LPAR; e = complex_expr; RPAR; list(NEWLINE) { e }

%%
