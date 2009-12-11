import Control.Monad (when)
import System.Random (getStdGen, randomRs, RandomGen)
import Test.StrictBench

expmod base exp m | exp == 0 = 1
                  | even exp = square (expmod base (exp `div` 2) m) `mod` m
                  | otherwise = (base * expmod base (exp - 1) m) `mod` m
    where square x = x * x

fast_prime :: RandomGen g => Integer -> Int -> g -> Bool
fast_prime n times g = all fermat_test (take times (randomRs (1, n - 1) g))
    where fermat_test a = expmod a n n == a

timed_prime_test n = do
  -- time the prime checking
  g <- getStdGen
  let is_prime = fast_prime n 15 g
  t1 <- time $ is_prime

  -- output it niely
  putStr (show n)
  when is_prime (putStr $ " -> " ++ show t1 ++ "ms")
  putStrLn ""

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
Runtimes:
1009 -> 1.148ms
1013 -> 1.179ms
1019 -> 1.213ms
10007 -> 1.413ms
10009 -> 1.351ms
10037 -> 1.408ms
100003 -> 1.656ms
100019 -> 1.769ms
100043 -> 1.668ms
1000003 -> 1.932ms
1000033 -> 1.91ms
1000037 -> 1.963ms

The direct interpretation of O(log n) is that testing primes near a
million should take about log 1000 = 3 times as long as primes near a
thousand. The data does not agree with this. The reason why is that
all O(log n) growth guarantees is that when multiplying the problem
size, the solution time only increases by a constant.

from 1e3 to 1e4: ~0.211ms
from 1e4 to 1e5: ~0.307ms
from 1e5 to 1e6: ~0.237ms

this seems to agree with the data.

-}
