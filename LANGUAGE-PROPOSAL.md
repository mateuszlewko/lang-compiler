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
    (0..)
    .map (\n -> n * n)                   -- All natural numbers squared
    .takeWhile (\nSqr -> nSqr < upper)   -- Below upper limit
    .filter (\nSqr -> is_odd(n_squared)) -- That are odd
    .fold (\acc nSqr -> acc + nSqr) 0    -- Sum them
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

### Option 3 (haskell)

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

### Option 3 (haskell)

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