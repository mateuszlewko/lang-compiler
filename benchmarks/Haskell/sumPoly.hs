sumPoly :: Num a => Int -> a ->  a -> a
sumPoly 0 curr _ = curr 
sumPoly n curr x = sumPoly (n - 1) (curr + x) $! x

main = do 
  putStrLn (show (sumPoly 100000000 12 2))