module Main where

filteredAccumulate filter combiner nullValue term a next b = f a
    where f a | a > b     = nullValue
              | filter a  = combiner (term a) (f $ next a)
              | otherwise = f (next a)

-- a. sum of the squares of the prime numbers in the interval a to b
f a b = filteredAccumulate isPrime (+) 0 square a (+1) b
    where square x = x * x

-- b. product of all the positive integers less than n that are relatively prime to n
g n = filteredAccumulate relPrime (*) 1 id 1 (+1) (n-1)
    where relPrime i = gcd i n == 1

main = do putStrLn ("f 1 10 = " ++ (show $ f 1 10))
          putStrLn ("g 10 = " ++ (show $ g 10))

-- extra things
isPrime n = smallestDivisor n == n

smallestDivisor :: Integer -> Integer
smallestDivisor n = findDivisor n 2
    where divides a b = b `mod` a == 0
          square x = x * x
          findDivisor n test | square test > n = n
                             | divides test n = test
                             | otherwise = findDivisor n (test + 1)
