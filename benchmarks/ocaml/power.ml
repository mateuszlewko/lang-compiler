let rec power a n =
  if n = 0
  then 1
  else a * (power a (n - 1))

let _ =
  print_int (power 3 200000);
  print_newline ()