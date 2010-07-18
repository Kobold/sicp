{-
Exercise 1.38.  In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus
Continuis, which included a continued fraction expansion for e - 2, where e is the base of the
natural logarithms. In this fraction, the Ni are all 1, and the Di are successively
1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses your cont-frac procedure from
exercise 1.37 to approximate e, based on Euler's expansion.

*Main> cfEuler 1
1.0
*Main> cfEuler 10
0.7182817182817183
*Main> cfEuler 100
0.7182818284590453
-}

module Main where

contFrac n d k = f 1
    where f i = if i <= k then n i / (d i + f (i + 1))
                          else 0

rotate :: [[a]] -> [a]
rotate xs = map head xs ++ rotate (map tail xs)

di :: [Integer]
di = 0 : rotate [repeat 1, [2,4..], repeat 1]

cfEuler :: Int -> Double
cfEuler = contFrac (const 1) (\i -> fromIntegral (di !! i))
