
module StructTest

// [<Struct>]
type Lst = None | Next of int * Lst

[<Struct>]
type Pair<'a, 'b> = Pair of 'a * 'b  

let empty = None 

let push x curr = Next (x, curr)

let pop = function 
          | Next (x, xs) -> xs 
          | None -> failwith "empty"

let l1 = empty |> push 1 |> push 2 |> push 3 |> pop 

printfn "%A" l1

let v1 = Pair (1, Pair (2, ()))
printfn "%A" v1        