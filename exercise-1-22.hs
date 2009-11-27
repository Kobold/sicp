import Control.Monad
import Test.StrictBench

smallest_divisor :: Integer -> Integer
smallest_divisor n = find_divisor n 2
    where divides a b = b `mod` a == 0
          square x = x * x
          find_divisor n test | square test > n = n
                              | divides test n = test
                              | otherwise = find_divisor n (test + 1)

is_prime n = smallest_divisor n == n

timed_prime_test n = do
  putStr (show n)
  t1 <- time $ is_prime n
  if is_prime n then putStrLn $ " -> " ++ show t1 ++ "ms"
                else putStrLn ""

search_for_primes start stop = forM_ [start..stop] timed_prime_test

{-
1009 -> 0.151ms
1013 -> 0.15ms
1019 -> 0.152ms

10007 -> 0.527ms
10009 -> 0.472ms
10037 -> 0.476ms

100003 -> 1.523ms
100019 -> 1.628ms
100043 -> 1.602ms

1000003 -> 4.968ms
1000033 -> 4.814ms
1000037 -> 4.615ms

sqrt 10 = 3.1622776601683795
0.5ms / 0.15ms = 3.333...
1.6ms / 0.5ms  = 3.2
4.8ms / 1.6ms  = 2.999...

This supports the big O prediction.
-}
