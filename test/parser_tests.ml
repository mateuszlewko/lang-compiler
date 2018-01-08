open Core
open Lang_compiler
open Ast
open OUnit

let tests = "parser" >:::
[
  (* --- test case --- *)
  "\n\tlet \n\tand same line var \n\tand vars or literals in next indented lines" >:: (fun () -> 
    let src = 
"
let fn a b = a
  fn
  2
  a

"   in 
    let prog = 
      (Ast.Prog
        [(Ast.LetExp (
          "fn", ["a"; "b"], 
            (Some (Ast.VarExp "a")),
            (Some [(Ast.VarExp "fn"); (Ast.LitExp (Ast.Int 2)); (Ast.VarExp "a")])
          )
         )
        ]) 
    in assert_equal (Parser.prog_of_string src) prog
  );

  (* --- test case --- *)
  "\n\tlet \n\tand vars or literals or applications in next indented lines" >:: (fun () -> 
    let src = 
"
let fn a b =
    fn a b
    bbbb
    aa
    
    3
    fn 2

"   in 
    let prog = 
      (Ast.Prog
        [(Ast.LetExp ("fn", ["a"; "b"], None,
            (Some [(Ast.AppExp ((Ast.VarExp "fn"),
                      [(Ast.VarExp "a"); (Ast.VarExp "b")], None));
                    (Ast.VarExp "bbbb"); (Ast.VarExp "aa");
                    (Ast.LitExp (Ast.Int 3));
                    (Ast.AppExp ((Ast.VarExp "fn"), [(Ast.LitExp (Ast.Int 2))],
                        None))
                  ])
            ))
        ])
 
    in assert_equal (Parser.prog_of_string src) prog
  );

  (* --- test case --- *)
  "complex program (multiple lets with infix op and application)" >:: (fun () -> 
    let src = 
" 
let fn a b =
    (fn2 a b)
      a b
    bbbb
    aa

    (a + b) + 2 - (7 + 5)

    3
    fn 2
      3

let adder a b c =
  adder a b

a + b + 2
"     
    in 
    let prog = 
      (Ast.Prog
        [(Ast.LetExp ("fn", ["a"; "b"], None,
            (Some [(Ast.AppExp (
                      (Ast.AppExp ((Ast.VarExp "fn2"),
                          [(Ast.VarExp "a"); (Ast.VarExp "b")], None)),
                      [],
                      (Some [(Ast.AppExp ((Ast.VarExp "a"), [(Ast.VarExp "b")],
                                None))
                              ])
                      ));
                    (Ast.VarExp "bbbb"); (Ast.VarExp "aa");
                    (Ast.InfixOp ("-",
                        (Some (Ast.InfixOp ("+",
                                (Some (Ast.InfixOp ("+", (Some (Ast.VarExp "a")),
                                          (Some (Ast.VarExp "b"))))),
                                (Some (Ast.LitExp (Ast.Int 2)))))),
                        (Some (Ast.InfixOp ("+", (Some (Ast.LitExp (Ast.Int 7))),
                                (Some (Ast.LitExp (Ast.Int 5))))))
                        ));
                    (Ast.LitExp (Ast.Int 3));
                    (Ast.AppExp ((Ast.VarExp "fn"), [(Ast.LitExp (Ast.Int 2))],
                        (Some [(Ast.LitExp (Ast.Int 3))])))
                    ])
            ));
          (Ast.LetExp ("adder", ["a"; "b"; "c"], None,
              (Some [(Ast.AppExp ((Ast.VarExp "adder"),
                        [(Ast.VarExp "a"); (Ast.VarExp "b")], None))
                      ])
              ));
          (Ast.InfixOp ("+",
              (Some (Ast.InfixOp ("+", (Some (Ast.VarExp "a")),
                      (Some (Ast.VarExp "b"))))),
              (Some (Ast.LitExp (Ast.Int 2)))))
          ])

 
    in assert_equal (Parser.prog_of_string src) prog
  );
]

(* let _ = begin
  (* enable pretty error messages *)
  Parser.pp_exceptions ();
  
  printf "Tokens:\n";
  (* List.iter Lexer.all_of_token ~f:(fun t ->
    Lexer.show_token t
    |> printf "  %s\n"); *)

  let s1 = "
let fn = a 

let fn a b =
  fn a
  fn a b
  b


let a = 4" in
  
  let s2 = 
"
let fn a b = a
  fn
  2
  a

" 
  in 

  let s3 = 
"   
let fn a b =
    fn a b
    bbbb
    aa
    
    3
    fn 2

" 
  in 

  let s4 = 
" 
let fn a b =
    (fn2 a b)
      a b
    bbbb
    aa

    (a + b) + 2 - (7 + 5)

    3
    fn 2
      3

let adder a b c =
  adder a b

a + b + 2
" 
  in 
  List.iter [s2; s3; s4] ~f:(fun s ->
    printf "\nTrying to parse \"%s\".\n" s;
    printf "res: %s\n" (Parser.prog_of_string s |> show_program);
    printf "-> success!\n";
  );
  ()
end *)
