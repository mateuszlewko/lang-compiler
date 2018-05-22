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
%token QUOTE COMMA COLON SEMICOL DOT
%token EQ
%token INDENT DEDENT NEWLINE EOF
%token EXTERNAL
%token ARRAY_OPEN ARRAY_CLOSE
%left OPERATOR

%token LEQ GEQ LE GE NEQ PLUS MINUS DIV MULT AND OR
%left AND OR
%left LEQ GEQ EQ LE GE NEQ 
%left PLUS MINUS
%left DIV MULT
/* %nonassoc UMINUS */

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

%inline oper:
  | AND { "&&" }
  | OR { "||" }
  | LEQ { "<=" }
  | LE { "<" }
  | GE { ">" }
  | GEQ { ">=" }
  | EQ { "=" }
  | NEQ { "<>" }
  | PLUS { "+"}
  | MINUS { "-"}
  | MULT { "*"}
  | DIV { "/"}
  | o = OPERATOR { o }

infix_op:
  | l = value_expr; o = oper; r = value_expr
    { InfixOp (o, Some l, Some r) }
  /* | l = value_expr; EQ; r = value_expr
    { InfixOp ("=", Some l, Some r) } */

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

%inline literal:
  | UNIT { Unit }
  /* | MINUS i = INT %prec UMINUS { Int (-i) } */
  | i = INT { Int i }
  | b = BOOL { Bool b }
  | ar = array_lit { ar }

field_get_expr:
  | s = SYMBOL; DOT; field = SYMBOL 
    { FieldGetExp (VarExp s, field) }
  | e = field_get_expr; DOT; field = SYMBOL 
    { FieldGetExp (e, field) }
  | e = simple_expr; DOT; field = SYMBOL 
    { FieldGetExp (e, field) }
  | LPAR; e = value_expr; RPAR; DOT; field = SYMBOL 
    { FieldGetExp (e, field) }

record_decl_field: s = SYMBOL; ta = type_anot { s, ta }

ignore_indent: 
  | INDENT+; ignore_indent  { }
  | DEDENT+; ignore_indent  { }
  | NEWLINE+; ignore_indent { }
  | { }

record_decl_sep: 
  | NEWLINE+; INDENT*; DEDENT*; SEMICOL; { }
  | NEWLINE+; INDENT*; DEDENT*          { }
  | NEWLINE*; SEMICOL; NEWLINE* INDENT*; DEDENT*        { }

record_fields:
 | f = record_decl_field; ignore_indent; RCURLY { [f] }
 | f = record_decl_field; record_decl_sep; fs = record_fields { f::fs }

record_decl:
  TYPE; name = SYMBOL; EQ; ignore_indent; LCURLY; ignore_indent;
  fields = record_fields; NEWLINE*; DEDENT* { RecordType (name, fields) }

record_literal_field: 
  | s = SYMBOL; EQ; exp = value_expr { s, exp }
  | s = SYMBOL { s, VarExp s }

record_literal_fields:
 | f = record_literal_field; ignore_indent; RCURLY { [f] }
 | f = record_literal_field; record_decl_sep; 
   fs = record_literal_fields { f::fs }

record_literal: 
  LCURLY; ignore_indent; fields = record_literal_fields
  { RecordLiteral fields }

record_update: 
  LCURLY; ignore_indent; e = value_expr; ignore_indent; WITH; ignore_indent; 
  fields = record_literal_fields
  { RecordWithExp (e, fields) }

simple_expr:  
  | l = literal                 { LitExp l }
  | s = nested_sym              { VarExp s }
  | i = infix_op                { i }
  | e = field_get_expr          { e }
  | r = record_literal          { r }
  | LPAR; a = application; RPAR { a }
  | LPAR; e = simple_expr; RPAR { e }

value_expr:
  | e = simple_expr    { e }
  /*| e = field_get_expr { e }*/
  | e = application    { e }

complex_expr:
  | e = record_update; NEWLINE* { e }
  | e = record_literal; NEWLINE* { e }
  | l = letexp; NEWLINE* { l }
  | e = if_exp; NEWLINE* { e }
  | s = value_expr; NEWLINE+ { s }
  | LPAR; e = complex_expr; RPAR; NEWLINE* { e }

external_expr: EXTERNAL; s = SYMBOL; t = type_anot; NEWLINE+ { Extern (s, t) }

type_decl: 
  | r = record_decl { r }

top_expr:
 | m = module_exp    { m          }
 | o = open_exp      { o          }
 | e = complex_expr  { Expr e     }
 | e = external_expr { e          }
 | t = type_decl     { TypeDecl t }

%%
