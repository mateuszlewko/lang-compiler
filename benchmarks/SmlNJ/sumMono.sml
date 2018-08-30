fun sumPoly (add : int -> int -> int) n (curr : int) (x : int) =
  if n = 0 
  then curr 
  else sumPoly add (n - 1) (add curr x) x

val _ = 
  print (Int.toString (
    sumPoly (fn (x:int) => fn (y:int) => x + y) 200000000 12 2));
 
  OS.Process.exit(OS.Process.success)

