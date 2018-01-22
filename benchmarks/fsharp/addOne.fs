let rec addOne x n =
    if n = 0
    then x 
    else addOne (x + 1) (n - 1)

printfn "%d" (addOne 0 1000000000)