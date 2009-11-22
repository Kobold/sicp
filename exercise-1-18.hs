import Test.QuickCheck

double =  (* 2)
halve = (`div` 2)

-- an iterative multiplication taking a logarithmic number of additions
fast_mult = go 0
    where go accum a b | b == 0    = accum
                       | even b    = go accum       (double a) (halve b)
                       | otherwise = go (accum + a) a          (b - 1)

prop_eq a b = b >= 0 ==>
              a * b == fast_mult a b
    where types = (a :: Integer, b :: Integer)

main = do
  quickCheck prop_eq
