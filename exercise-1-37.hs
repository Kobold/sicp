{-
1 / phi ~= 0.618033988

k must be 11 to get an approximation that is accurate to 4 decimal places

*Main> contFracR (const 1) (const 1) 10
0.6179775280898876
*Main> contFracR (const 1) (const 1) 11
0.6180555555555556
-}

module Main where

-- recursive
contFracR n d k = f 1
    where f i = if i <= k then n i / (d i + f (i + 1))
                          else 0

-- iterative
contFracI n d k = f k 0
    where f i accum = if i > 0 then f (i - 1) (n i / (d i + accum))
                               else accum



