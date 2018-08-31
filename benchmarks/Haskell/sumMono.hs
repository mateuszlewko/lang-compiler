import GHC.Prim
import GHC.Exts

sumMono :: Int# -> Int# -> Int# -> Int# 
sumMono 0# curr _ = curr 
sumMono n curr x = sumMono (n -# 1#) (curr +# x) x

showUnboxedInt :: Int# -> String
showUnboxedInt n = (show $ I# n) ++ "#"

main = do 
  putStrLn (showUnboxedInt (sumMono 100000000# 12# 2#))