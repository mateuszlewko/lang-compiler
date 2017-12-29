open Core

type literal = Int of int | String of string | Bool of bool
[@@deriving show]

type expr = 
  | VarExp of string
  | LitExp of literal
  | LetExp of string * string list * expr option * expr list option
  | AppExp of expr * expr list * expr list option
  | Exps of expr list 
  | InfixOp of string * expr * expr    (* TODO: *)
  | IfExp of expr * expr * expr option (* TODO: *)
  [@@deriving show]

type toplevel = 
  | Expr of expr
  | InfixSet    (* TODO: *)
  | ModuleDecl  (* TODO: *)
  | TypeDecl    (* TODO: *)
  [@@deriving show]

let sprint_literal = 
  function
  | Int i    -> sprintf "Int %d" i 
  | String s -> sprintf "String %s" s 
  | Bool b   -> sprintf "Bool %b" b 

let sprint_option cont =
  function 
  | None -> "None"
  | Some x -> cont x |> sprintf "Some (%s)"

let rec sprint_expr_ast = 
  function 
  | VarExp v -> sprintf "VarExp %s" v
  | LitExp l -> sprint_literal l
  | LetExp (n, vs, line, es) -> 
      let args = "[" ^ String.concat ~sep:", " vs ^ "]" in 
      let fst_line = sprint_option sprint_expr_ast line in 
      let exprs = sprint_option sprint_exprs es in 
      sprintf "Let (%s, %s, %s, %s)" n args fst_line exprs 
  | AppExp (e, es1, es2) ->
      let e = sprint_expr_ast e in 
      let es1 = sprint_exprs es1 in
      let es2 = sprint_option sprint_exprs es2 in 
      sprintf "App (%s, %s, %s)" e es1 es2 
  | Exps es -> sprintf "Exps (%s)" (sprint_exprs es)
  | _ -> "unsupported"
and sprint_exprs es = 
 "[" ^ (List.map es ~f:sprint_expr_ast |> String.concat ~sep:";\n") ^ "]"
