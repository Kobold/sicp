module Main where
import Test.QuickCheck

-- a. recursive implementation of product
productR term a next b =
    if a > b then 1
             else term a * productR term (next a) next b

factorial = productR id 1 (+1)

wallis n = productR term 2 (+1) (2+n)
    where term x = let y = fromIntegral x in
                   if even x then y / (y + 1)
                             else (y + 1) / y

-- b. iterative implementation of product
productI term a next b = iter a 1
    where iter a result = if a > b then result
                                   else iter (next a) (term a * result)

prop_eq a b n = a < b && n > 0 ==>
              productI id a (+n) b == productR id a (+n) b
    where types = (a :: Integer, b :: Integer, n :: Integer)


main = do
    quickCheck prop_eq
    putStr "pi ~= "
    putStrLn (show $ 4 * wallis 500)
