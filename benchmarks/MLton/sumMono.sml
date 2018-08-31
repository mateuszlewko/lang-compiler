fun sum add n curr x =
  if n = 0 
  then curr 
  else sum add (n - 1) (add (curr, x)) x

val _ = 
  print (Int.toString (
    sum (fn (x:int, y:int) => x + y) 100000000 12 2))