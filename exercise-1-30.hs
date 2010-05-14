module Main where
import Prelude hiding (sum)

-- an iterative implementation of sum
sum term a next b = iter a 0
    where iter a result = 
              if a > b then result
                       else iter (next a) (result + term a)


-- boilerplate
cube x = x * x * x

main = do putStr "sum cube 1 (+1) 10 = "
          putStrLn (show $ sum cube 1 (+1) 10)
