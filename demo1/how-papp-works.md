# How partial application works

## Example

```fsharp
let adder a b c : int -> int -> int =
    let innerAdder d e = a + ... + e
    innerAdder
```

### 1) closure conversion

```fsharp
let adder a b c : int -> int -> int =
    let innerAdder a b c d e = a + ... + e
    (innerAdder a b c)
```

### 2) lambda lifting

```fsharp
let innerAdder.lifted a b c d e = a + ... + e

let adder a b c : int -> int -> int =
    let innerAdder = (innerAdder.lifted a b c)
```

### 3) partial application

```fsharp
let adder-env-1 env args_cnt b c d e = (adder env.a b c) d? e? ...
let adder-env-2 env args_cnt c d e = ...
let adder-env-3 env args_cnt d e = ...

let innerAdder-env-1 args_cnt b c d e = innerAdder env.a b c d e 
```