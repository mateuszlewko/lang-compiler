let rec power a n =
  if n = 0
  then 1
  else a * (power a (n - 1))

module A =
    module B =
        let testB =
            power 2 3

    let testB = "s"


    open B

    let x = 4

    // let testB a b = a b

    let testA x =
        List.length x
        power 1 2
        B.testB
        testB

open A
// open B 
B.testB


printfn "%d" (power 3 500000)