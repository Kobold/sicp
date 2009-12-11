import Test.StrictBench

smallest_divisor :: Integer -> Integer
smallest_divisor n = find_divisor 2
    where square x = x * x
          next n = if n == 2 then 3
                             else (n + 2)
          find_divisor test | square test > n = n
                            | n `mod` test == 0 = test
                            | otherwise = find_divisor (next test)

is_prime n = smallest_divisor n == n

timed_prime_test n = do
  putStr (show n)
  t1 <- time $ is_prime n
  if is_prime n then putStrLn $ " -> " ++ show t1 ++ "ms"
                else putStrLn ""

primes = [1009
         ,1013
         ,1019
         ,10007
         ,10009
         ,10037
         ,100003
         ,100019
         ,100043
         ,1000003
         ,1000033
         ,1000037]

main = mapM_ timed_prime_test primes

{-
Before divisor finding improvement
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

After improvement
1009 -> 0.123ms
1013 -> 9.9e-2ms
1019 -> 9.8e-2ms
10007 -> 0.29ms
10009 -> 0.281ms
10037 -> 0.295ms
100003 -> 0.963ms
100019 -> 1.169ms
100043 -> 0.88ms
1000003 -> 2.759ms
1000033 -> 2.707ms
1000037 -> 2.816ms

Improvement factor
0.15ms / 0.1ms  = 1.5
0.48ms / 0.29ms = 1.655
1.6ms / 1ms     = 1.6
4.8ms / 2.75ms  = 1.745

The expectation of running twice as fast is not quite met. The improvement
factor is only ~1.6, and this is because not all the processing time is
spent in testing. There's some overhead that exists either way. Amdahl's law.
-}
