import Test.QuickCheck

fast_expt_r :: (Floating a, Integral n) => a -> n -> a
fast_expt_r b n | n == 0    = 1
                | even n    = (fast_expt_r b (n `div` 2)) ** 2
                | otherwise = b * (fast_expt_r b (n - 1))

fast_expt_i :: (Floating a, Integral n) => a -> n -> a
fast_expt_i = go 1
    where go a b n | n == 0    = a
                   | even n    = go a       (b ** 2) (n `div` 2)
                   | otherwise = go (a * b) b        (n - 1)

approxEqual :: Float -> Float -> Bool
approxEqual a b = a == b || abs ((a - b) / b) < 0.00001

prop_eq_r b n = n >= 0 ==>
                (b ** fromIntegral n) `approxEqual` fast_expt_r b n
    where types = (b :: Float, n :: Int)
prop_eq_i b n = n >= 0 ==>
                (b ** fromIntegral n) `approxEqual` fast_expt_i b n
    where types = (b :: Float, n :: Int)

main = do
    quickCheck prop_eq_r
    quickCheck prop_eq_i
