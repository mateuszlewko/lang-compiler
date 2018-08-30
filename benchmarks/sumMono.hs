import GHC.Prim
import GHC.Exts

sumUnboxed :: Int# -> Int# -> Int# -> Int# 
sumUnboxed 0# curr _ = curr 
sumUnboxed n curr x = sumUnboxed (n -# 1#) (curr +# x) x

showUnboxedInt :: Int# -> String
showUnboxedInt n = (show $ I# n) ++ "#"

main = do 
  putStrLn (showUnboxedInt (sumUnboxed 100000000# 12# 2#))

