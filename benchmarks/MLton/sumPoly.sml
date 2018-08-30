fun sumPoly add n curr x =
  if n = 0 
  then curr 
  else sumPoly add (n - 1) (add (curr, x)) x

val _ = 
  print (Int.toString (
    sumPoly (fn (x:int, y:int) => x + y) 100000000 12 2))
  
  (* print (Real.toString (
    sumPoly (fn (x:real, y:real) => x + y) 1 12.1 1.9));
    
  OS.Process.exit(OS.Process.success) *)

