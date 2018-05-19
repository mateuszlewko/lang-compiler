let adder5 a b c d e = a + b + c + d + e 

let adder3 a b c : int -> int -> int = 
    adder5 a b c

let apply2 which (f3 : int -> int -> int) (g3 : int -> int -> int) () = 
    if which = 1
    then (f3 1 2) + (f3 3 10) 
    else if which = 2 
    then (f3 1 2) + (f3 3 11)
    else (f3 1 2) + (f3 3 12)

let rec test (fn : unit -> int) cnt = 
    if cnt = 0
    then fn ()
    else (
        fn ();
        test fn (cnt - 1)
    )

let main = test (apply2 2 (adder3 5 6 7) (adder3 8 9 10)) 100000000
let _ = printfn "%d" main