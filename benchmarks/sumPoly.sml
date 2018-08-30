fun sumPoly add n curr x =
  if n = 0 
  then curr 
  else sumPoly add (n - 1) (add curr x) x

val _ = 
  print (Int.toString (sumPoly (fn (x:int) => fn (y:int) => x + y) 100000000 12
  2));
  OS.Process.exit(OS.Process.success)

