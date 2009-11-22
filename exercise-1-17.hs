import Test.QuickCheck

double =  (* 2)
halve = (`div` 2)

-- a recursive multiplication taking a logarithmic number of additions
fast_mult a b | b == 0    = 0
              | even b    = double (fast_mult a (halve b))
              | otherwise = a + fast_mult a (b - 1)

prop_eq a b = b >= 0 ==>
              a * b == fast_mult a b
    where types = (a :: Integer, b :: Integer)

main = do
  quickCheck prop_eq
