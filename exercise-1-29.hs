module Main where
import Prelude hiding (sum)

-- integration using Simpson's Rule
simpson f a b n = (h / 3) * sum term 0 (+1) n
    where term k = coeff k * f (a + fromIntegral k * h)
          h = (b - a) / fromIntegral n
          coeff k | k == 0 || k == n = 1
                  | even k = 2
                  | otherwise = 4

-- boilerplate
sum term a next b =
    if a > b then 0
             else term a + sum term (next a) next b

cube x = x * x * x

main = do putStr "simpson cube 0 1 100 = "
          putStrLn (show $ simpson cube 0 1 100)
          putStr "simpson cube 0 1 1000 = "
          putStrLn (show $ simpson cube 0 1 1000)
