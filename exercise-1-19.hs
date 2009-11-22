import Test.QuickCheck

{-
Applying the transformation Tpq twice

a1 <- b0 q + a0 q + a0 p
b1 <- b0 p + a0 q


a2 <- b1 q + a1 q + a1 p
      (b0 p + a0 q) q + (b0 q + a0 q + a0 p) q + (b0 q + a0 q + a0 p) p
      b0 p q + a0 q^2 + b0 q^2 + a0 q^2 + a0 p q + b0 p q + a0 p q + a0 p^2
      b0 (q^2 + 2 p q) + a0 (q^2 + 2 p q) + a0 (p^2 + q^2)

b2 <- b1 p + a1 q
      (b0 p + a0 q) p + (b0 q + a0 q + a0 p) q
      b0 p^2 + a0 p q + b0 q^2 + a0 q^2 + a0 p q
      b0 (p^2 + q^2) + a0 (q^2 + 2 p q)

thus a transformation Tp'q' can be defined where

p' = p^2 + q^2
q' = q^2 + 2 p q
-}

-- fibonacci numbers in logarithmic time
fib n = f 1 0 0 1 n
    where f a b p q count | count == 0 = b
                          | even count = f a
                                           b
                                           (square p + square q)
                                           (square q + 2 * p * q)
                                           (count `div` 2)
                          | otherwise  = f (b * q + a * q + a * p)
                                           (b * p + a * q)
                                           p
                                           q
                                           (count - 1)
          square x = x * x

prop_fib n = n >= 2 ==>
             fib n == fib (n - 1) + fib (n - 2)
    where types = n :: Int

main = do
    quickCheck prop_fib
