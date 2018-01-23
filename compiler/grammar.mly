%{
open Core
open Ast

let to_exps fst_line rest = 
  let exps = Option.value rest ~default:[] in
  let exps = match fst_line with 
                  | None   -> exps 
                  | Some e -> e::exps
  in Exprs exps
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

/* %start <Ast.expr> top_let */
%start <Ast.program> program

%%

/* ending:
  | EOF {}
  | NEWLINE {} */

program: NEWLINE*; es = top_expr+; NEWLINE*; EOF { Prog (es) }

empty_line:
  | NEWLINE                     { }
  | INDENT; empty_line*; DEDENT { }

single_type_anot:
  | UNIT { "()" }
  /* | i = INT { string_of_int i } */
  | ts = SYMBOL { ts }

nested_sym:
  | s = NESTED_SYMBOL { s }
  | s = SYMBOL { s }

type_anot: COLON; t = separated_list(ARROW, single_type_anot+) { t }

typed_var:
  | s = UNIT; { "()", Some [["()"]]  }
  | LPAR; s = SYMBOL; t = type_anot?; RPAR { s, t }
  | s = SYMBOL; { s, None }
  /* | LPAR; t = option(typed_var); RPAR { t } */

module_exp: MODULE; s = SYMBOL; EQ; NEWLINE+; INDENT; es = top_expr+; DEDENT
            { Module (s, es) }

open_exp: OPEN; s = nested_sym; NEWLINE+ { Open s }

/* top_let:
  | NEWLINE; e = top_let { e }
  | e = letexp; NEWLINE? { e } */

letexp:
  | LET; is_rec = boption(REC); n = SYMBOL; vs = typed_var*;
    rett = type_anot?; EQ; e = value_expr?;
    NEWLINE*; es = indented?
    { LetExp (is_rec, (n, rett), vs, e, es) }

/* indent_cont: NEWLINE+; e = indented { e } */

application:
  | s = simple_expr; es1 = simple_expr+
    /* es2 = indent_cont?; */
    { AppExp (s, es1, None) }

infix_op:
  | l = value_expr; o = OPERATOR; r = value_expr
    { InfixOp (o, Some l, Some r) }
  | l = value_expr; EQ; r = value_expr
    { InfixOp ("=", Some l, Some r) }

else_exp: ELSE; exp = value_expr?; NEWLINE*; exps = indented?
          { to_exps exp exps }

elif_exp: ELIF; cond = value_expr; NEWLINE*; THEN; true_ex = value_expr?;
          NEWLINE*; true_exps = indented? 
          { cond, to_exps true_ex true_exps }

if_exp: IF; cond = value_expr; NEWLINE*; THEN; true_ex = value_expr?;
        NEWLINE*; true_exps = indented? NEWLINE*; elif_exps = elif_exp*; 
        else_ex = else_exp?; NEWLINE? 
        { IfExp (cond, to_exps true_ex true_exps, elif_exps, else_ex) }

%inline indented: INDENT; es = complex_expr+; DEDENT { es }

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
  | s = nested_sym { VarExp s }
  | i = infix_op { i }
  | LPAR; a = application; RPAR { a }
  | LPAR; e = simple_expr; RPAR { e }

value_expr:
  | e = application { e }
  | e = simple_expr { e }

complex_expr:
  | l = letexp; NEWLINE* { l }
  | e = if_exp; NEWLINE* { e }
  /* | a = application; NEWLINE* { a } */
  | s = value_expr; NEWLINE+ { s }
  | LPAR; e = complex_expr; RPAR; NEWLINE* { e }

external_expr: EXTERNAL; s = SYMBOL; t = type_anot; NEWLINE+ { Extern (s, t) }

top_expr:
 | m = module_exp { m }
 | o = open_exp { o }
 | e = complex_expr { Expr e }
 | e = external_expr { e }

%%
