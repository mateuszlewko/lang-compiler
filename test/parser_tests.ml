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