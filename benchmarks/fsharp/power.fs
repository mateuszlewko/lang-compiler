let rec power a n =
  if n = 0
  then 1
  else a * (power a (n - 1))

let rec a = printfn ""
let arr =
    [3;
       3;
  3]

let fn () = 4 + 5

let fn11  a =
    let x = printfn "Does it print?"

    12

let fn2 = fn a

module A =
    module B =
        let testB =
            power 2 3

    let testB = "s"


    open B
    let _ = printf "3"

    let x = 4

    // let testB a b = a b

    let testA x =
        List.length x
        power 1 2
        B.testB
        testB

// open A
// open B
// B.testB


printfn "%d" (power 3 100000)