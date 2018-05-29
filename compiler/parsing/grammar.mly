%{
open Core
open Ast
open BatPervasives

let merge_multiline fst_line rest = 
  let exps = Option.value rest ~default:[] in
  match fst_line with 
  | None   -> exps 
  | Some e -> e::exps

let to_exps fst_line rest = merge_multiline fst_line rest |> (fun x -> Exprs x)
%}

%token <string> SYMBOL
%token <string> NESTED_SYMBOL
%token <int> INT
%token <bool> BOOL
%token <string> STRING
%token <string> OPERATOR
%token <char> KWD
%token LET REC IF ELSE ELIF THEN MODULE TYPE OPEN 
%token CLASS INSTANCE WHERE WHEN
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

%start <Ast.program> program

%%

program: NEWLINE*; es = top_expr+; NEWLINE*; EOF { Prog (es) }

empty_line:
  | NEWLINE                     { }
  | INDENT; empty_line*; DEDENT { }

single_type_anot:
  | UNIT        { "()" }
  | ts = SYMBOL { ts   }

nested_sym:
  | s = NESTED_SYMBOL { s }
  | s = SYMBOL { s }

basic_type_anot: 
  | s = single_type_anot+ { Single s }
  | t1 = basic_type_anot; ARROW; 
    ts = separated_nonempty_list(ARROW, basic_type_anot) 
    { Fun (t1::ts) }
  | LPAR; t = basic_type_anot; RPAR { t }

single_classes_anot: 
  | t = basic_type_anot; COLON; c = SYMBOL { t, [c] }
  | t = basic_type_anot; COLON; LPAR; cs = separated_list(COMMA, SYMBOL); RPAR
    { t, cs }
  
classes_anots:
  | WHEN; cs = separated_list(COMMA, single_classes_anot) { cs }
  | { [] }

raw_type_anot: 
  basic = basic_type_anot; classes = classes_anots { { basic; classes } }

type_anot: COLON; t = raw_type_anot { t }

typed_var:
  | s = UNIT; { "()", Some { basic = Single ["()"]; classes = [] } }
  | LPAR; s = SYMBOL; t = type_anot?; RPAR { s, t }
  | s = SYMBOL; { s, None }

module_exp: MODULE; s = SYMBOL; EQ; NEWLINE+; INDENT; es = top_expr+; DEDENT
            { Module (s, es) }

open_exp: OPEN; s = nested_sym; NEWLINE+ { Open s }

letexp:
  | LET; is_rec = boption(REC); n = SYMBOL; args = typed_var*;
    ret_t = type_anot?; EQ; e = value_expr?;
    NEWLINE*; es = indented?
    { { name = n; is_rec; ret_t; args; body = merge_multiline e es } }

application:
  | s = simple_expr; es1 = simple_expr+ { AppExp (s, es1) }

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
    { InfixOp (o, l, r) }

else_exp: ELSE; exp = value_expr?; NEWLINE*; exps = indented?
          { to_exps exp exps }

elif_exp: ELIF; cond = value_expr; NEWLINE*; THEN; true_ex = value_expr?;
          NEWLINE*; true_exps = indented? 
          { cond, to_exps true_ex true_exps }

if_exp: IF; cond = value_expr; NEWLINE*; THEN; true_ex = value_expr?;
        NEWLINE*; true_exps = indented? NEWLINE*; elif_exps = elif_exp*; 
        else_ex = else_exp?; NEWLINE? 
        { IfExp { cond 
                ; then_ = to_exps true_ex true_exps
                ; elifs = elif_exps
                ; else_ = else_ex } }

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

with_exp: e = value_expr; WITH; ignore_indent { e }

record_literal: 
| LCURLY; ignore_indent; fields = record_literal_fields
  { RecordLiteral fields }
| LCURLY; ignore_indent; w = with_exp; ignore_indent; 
  fields = record_literal_fields
  { RecordWithExp (w, fields) }

class_type:
  | LPAR; s = SYMBOL; COLON; parents = separated_list(COMMA, SYMBOL) RPAR; WHERE
    { s, parents }
  | s = SYMBOL; COLON; p1 = SYMBOL WHERE { s, [p1] }
  | s = SYMBOL WHERE { s, [] }

class_method_decl: s = SYMBOL; t = type_anot { s, t }

more_methods: 
  | NEWLINE+; INDENT; ms = separated_list(NEWLINE+, class_method_decl); DEDENT 
    NEWLINE+ { ms }
  | NEWLINE+ { [] }

class_declaration:
  CLASS; name = SYMBOL; t = class_type; d1 = class_method_decl?;
  ms = more_methods { 
    let type_name, parent_classes = t in 
    let declarations = match d1 with 
                       | Some d1 -> d1::ms | None -> ms in 
                       
    { name; type_name; parent_classes; declarations } 
  }

class_instance: 
  INSTANCE; class_name = SYMBOL; type_ = raw_type_anot; WHERE;
  NEWLINE+; INDENT; definitions = letexp+; DEDENT
  { { class_name; type_; definitions } }

simple_expr:  
  | l = literal                 { LitExp l }
  | s = nested_sym              { VarExp s }
  | i = infix_op                { i }
  | e = field_get_expr          { e }
  | r = record_literal          { r }
  | LPAR; a = application; RPAR { a }
  | LPAR; e = simple_expr; RPAR { e }

value_expr:
  | e = simple_expr { e }
  | e = application { e }

complex_expr:
  | e = record_literal; NEWLINE*           { e        }
  | l = letexp; NEWLINE*                   { LetExp l }
  | e = if_exp; NEWLINE*                   { e        }
  | s = value_expr; NEWLINE+               { s        }
  | LPAR; e = complex_expr; RPAR; NEWLINE* { e        }

external_expr: EXTERNAL; s = SYMBOL; t = type_anot; NEWLINE+ { Extern (s, t) }

type_decl: 
  | r = record_decl { r }

top_expr:
 | m = module_exp        { m          }
 | o = open_exp          { o          }
 | e = complex_expr      { Expr e     }
 | e = external_expr     { e          }
 | t = type_decl         { TypeDecl t }
 | c = class_declaration { Class c    }
 | i = class_instance    { Instance i }

%%
