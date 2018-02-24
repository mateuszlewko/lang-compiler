# The Lang Language Proposal

## Let, Let rec and indent-sensitive syntax

### Option 1 for type annotations

```fsharp
val adder : int -> int -> int
let adder x y = x + y

val repeat : (() -> ()) -> int -> ()
let rec repeat fn times : () =
  if times > 0
  then
    fn ()
    repeat fn (times - 1)
```

### Option 2 for type annotations

```fsharp
let adder (x : int) (y : int) : int = x + y

val repeat : (unit -> unit) -> int -> unit
let rec repeat fn times =
  if times > 0
  then
    fn ()
    repeat fn (times - 1)
```

## Higher order functions and lambda functions

### Option 1 (fsharp / scala inspired)

```fsharp
let sumOfSquaredOddNumbers =
    (0..)
    |> map (n => n * n)                   // All natural numbers squared
    |> takeWhile (nSqr => nSqr < upper)   // Below upper limit
    |> filter (nSqr => is_odd(n_squared)) // That are odd
    |> fold (acc nSqr => acc + nSqr) 0    // Sum them
```

### Option 2 (rust / haskell inspired)

```haskell
let sumOfSquaredOddNumbers2 =
    -- (.) is just a function application operator
    (0..) 
    .map (\n -> n * n)                   -- All natural numbers squared
    .takeWhile (\nSqr -> nSqr < upper)   -- Below upper limit
    .filter (\nSqr -> is_odd(n_squared)) -- That are odd
    .fold (\acc nSqr -> acc + nSqr) 0    -- Sum them

let isEmpty set = 
    set . size = 0

let isEmptyGeneric container =
    container.size = 0
```

### Option 3 (fsharp / ocaml)

```fsharp
let sumOfSquaredOddNumbers =
    (0..)
    |> map (fun n -> n * n)                   // All natural numbers squared
    |> takeWhile (fun nSqr -> nSqr < upper)   // Below upper limit
    |> filter (fun nSqr -> is_odd(n_squared)) // That are odd
    |> fold (fun acc nSqr -> acc + nSqr) 0    // Sum them
```

## Algebraic data types (variants)

### Option 1 (haskell)

```haskell
type Tree = Empty
          | Leaf Int
          | Node Tree Tree

type Shape = 
    | Circle Float Float Float
    | Square Float
    | Rectangle Float Float
```

### Option 2 (fsharp 1)

```fsharp
type Tree = Empty
          | Leaf of int
          | Node of Tree * Tree
```

### Option 3 (fsharp 2)

```fsharp
type Tree = Empty
          | Leaf of int
          | Node of (Tree, Tree)
```

### Option 4 (ocaml 1)

```ocaml
type tree = Circle of float * float * float
          | Square of float
          | Rectangle of float * float 
```

### Option 5 (ocaml 2)

```ocaml
type tree = Circle of (float, float, float)
          | Square of float
          | Rectangle of (float, float)
```

## Pattern matching

### Option 1 (fsharp)

```fsharp
let sumOf =
    function
    | Circle (x, y, z) -> x + y + z
    | Square x         -> x * x
    | Rectangle (a, b) -> a + b
```

### Option 2 (haskell)

```fsharp
let sumOf =
    function
    | Circle x y z  -> x + y + z
    | Square x      -> x * x
    | Rectangle a b -> a + b
```

### Option 3 (haskell / fsharp)

```haskell
let sumOf shape =
    case shape of
    | Circle x y z  -> x + y + z
    | Square x      -> x * x
    | Rectangle a b -> a + b


let sumOf shape1 shape2 =
    case shape1 of
    | Circle x y z  -> 
        let sumOf shape =
            case shape1 of
            | Circle x y z  -> x + y + z
            | Square x      -> x * x
            | Rectangle a b -> a + b
        
        sumOf shape2 + x + y + z
    | Square x      -> x * x
    | Rectangle a b -> a + b
```

### Option 4 (haskell)

```haskell
let sumOf shape =
  case shape of
    Circle x y z  -> x + y + z
    Square x      -> x * x
    Rectangle a b -> a + b

let sumOf shape1 shape2 =
    case shape1 of
        Circle x y z  -> 
            let sumOf shape =
                case shape1 of
                    Circle x y z  -> x + y + z
                    Square x      -> x * x
                    Rectangle a b -> a + b

            sumOf shape2 + x + y + z
        Square x      -> x * x
        Rectangle a b -> a + b
```

## Named and optional arguments (as in ocaml)

```ocaml 
                      (* argument name, when calling function *)
                        /    (* arg name in function scope (optional) *)
                       /       /
let transform ~shape ~x:dx ~y:dy ?reverse=false ?handler =
    case shape with                               \
      Circle x y -> Circle (x + dx) (y + dy)    (* has option type *)  
    | Square x y -> 
        case handler of 
        | Some handler -> handler (x + dx) (y + dy)
        | None         -> Sqaure 0 0
```

## Record types

```fsharp
type User = { name   : string
              age    : int
              height : float
              status : Status
            }

type Rectangle = { a : int; b : int; posX : int; posY : int
                 ; color : Color }
```

## Higher order types (type classes / traits)

## List comprehension

## Modules

```fsharp
open MyLibrary

//                      package name and explicit version (used by main package repository)
//                     /
//    vvvvvvvvvvvvvvvvv
open "tools-lib.2.10.12" ToolsLib

//                            package directory
//                           /
//    vvvvvvvvvvvvvvvvvvvvvvv
open "~/packaged/otherPackage" qualified OtherPackage.{Math}

//                        package local directory (relative to file location)
//                       /
//    vvvvvvvvvvvvvvvvvvv
open "./libs/localPackage" LocalPackage

//                                package on github
//                                                \
//    vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
open "https://github.com/mateuszlewko/lang-compiler"
      qualified CompilerTools.{AST} hiding AST.Expr

module A =
    module B =
      let adder x y = x + y
      let sub x y = x - y
      let testA = 11

    let testA x =
      B.adder 21 x

let testA x y = x + y

open A hiding testA
open B.{adder, sub}
```

## Exceptions in function types

## Basic parallelism (Coroutines, processes, locks)

## Basic Garbage Collector

### Option 1

There should be one.

### Option 2

There won't be any :(