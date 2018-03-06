let duration f = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f ()
    printfn "Elapsed Time: %.3f" (float timer.ElapsedMilliseconds / 1000.0)
    returnValue 

let getAdder x = 
    let adder x y z = 
        let innerAdder x y z w q = x + y + z + w + q
        in innerAdder x y z 
    in adder x 

let fastAdder x y z w q = x + y + z + w + q

let n = 100 * 1000 * 1000;
let test1 () = 
    let mutable x = 0
    for i in 1..n do 
        x <- getAdder i (i + 1) (i * i) (-i) i

    x

printfn "%d" <| duration test1

let test2 () = 
    let mutable x = 0
    for i in 1..n do 
        x <- fastAdder i (i + 1) (i * i) (-i) i

    x

printfn "%d" <| duration test2