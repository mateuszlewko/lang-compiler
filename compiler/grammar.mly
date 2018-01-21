%{
open Core
open Ast
%}

%token <string> SYMBOL
%token <string> NESTED_SYMBOL
%token <int> INT
%token <bool> BOOL
%token <string> STRING
%token <string> OPERATOR
%token <char> KWD
%token LET REC IF ELSE ELIF THEN MODULE TYPE OPEN
%token PIPE FUNCTION MATCH WITH ARROW UNIT
%token LPAR RPAR LBRACKET RBRACKET LCURLY RCURLY
%token QUOTE COMMA COLON SEMICOL
%token EQ
%token INDENT DEDENT NEWLINE EOF
%token EXTERNAL
%token ARRAY_OPEN ARRAY_CLOSE
%left OPERATOR

%start <Ast.expr> top_let
%start <Ast.program> program

%%

program: option(NEWLINE); es = top_expr+; option(NEWLINE); EOF { Prog (es) }

single_type_anot:
  | UNIT { "()" }
  | t = SYMBOL { t }

nested_sym:
  | s = NESTED_SYMBOL { s }
  | s = SYMBOL { s }

type_anot: COLON; t = separated_list(ARROW, single_type_anot) { t }

typed_var:
  | s = UNIT; { "()", Some ["()"]  }
  | LPAR; s = SYMBOL; t = type_anot?; RPAR { s, t }
  | s = SYMBOL; { s, None }
  /* | LPAR; t = option(typed_var); RPAR { t } */

module_exp: MODULE; s = SYMBOL; EQ; NEWLINE+; INDENT; es = top_expr+; DEDENT
            { Module (s, es) }

open_exp: OPEN; s = nested_sym; NEWLINE+ { Open s }

top_let:
  | NEWLINE; e = top_let { e }
  | e = letexp; NEWLINE? { e }

letexp:
  | LET; is_rec = boption(REC); n = SYMBOL; vs = typed_var*;
    rett = type_anot?; EQ; e = simple_expr?;
    NEWLINE+; es = indented?
    { LetExp (is_rec, (n, rett), vs, e, es) }

application:
  | s = simple_expr; es1 = simple_expr+; option(NEWLINE);
    es2 = indented?; NEWLINE?
    { AppExp (s, es1, es2) }
  | s = simple_expr; NEWLINE;
    es2 = indented; NEWLINE?
    { AppExp (s, [], Some es2) }

infix_op:
  | l = simple_expr; o = OPERATOR; r = simple_expr
    { InfixOp (o, Some l, Some r) }
  | l = simple_expr; EQ; r = simple_expr
    { InfixOp ("=", Some l, Some r) }

else_exp: ELSE; exp = simple_expr {exp}

elif_exp: ELIF; cond = simple_expr; NEWLINE*; THEN; true_ex = simple_expr;
          NEWLINE? { cond, true_ex }

if_exp: IF; cond = simple_expr; NEWLINE*; THEN; true_ex = simple_expr;
        NEWLINE?; elif_exps = elif_exp*; else_ex = else_exp?;
        NEWLINE? { IfExp (cond, true_ex, elif_exps, else_ex) }

indented: INDENT; es = complex_expr*; DEDENT { es }

%inline white_space:
  | NEWLINE { }
  | INDENT { }
  | DEDENT { }

%inline array_elem:
  | white_space*; e = simple_expr; white_space* { e }
  | white_space*; e = complex_expr; white_space* { e }

array_lit: ARRAY_OPEN; es = separated_list(SEMICOL, array_elem); ARRAY_CLOSE;
           { Array es }

literal:
  | UNIT { Unit }
  | i = INT { Int i }
  | b = BOOL { Bool b }
  | ar = array_lit { ar }

simple_expr:
  | l = literal { LitExp l }
  | i = infix_op { i }
  | LPAR; a = application; RPAR { a }
  | s = nested_sym { VarExp s }
  | LPAR; e = simple_expr; RPAR { e }

complex_expr:
  | l = letexp; NEWLINE* { l }
  | e = if_exp; NEWLINE* { e }
  | a = application; NEWLINE* { a }
  | s = simple_expr; NEWLINE+ { s }
  | LPAR; e = complex_expr; RPAR; NEWLINE* { e }

external_expr: EXTERNAL; s = SYMBOL; t = type_anot; NEWLINE+ { Extern (s, t) }

top_expr:
 | m = module_exp { m }
 | o = open_exp { o }
 | e = complex_expr { Expr e }
 | e = external_expr { e }

%%
