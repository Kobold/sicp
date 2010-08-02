{-
Exercise 1.39.  A continued fraction representation of the tangent function was published in 1770
by the German mathematician J.H. Lambert:

tan x = x / (1 - x^2 / (3 - x^2 / (5 - ...)))

where x is in radians. Define a procedure (tan-cf x k) that computes an approximation to the
tangent function based on Lambert's formula. K specifies the number of terms to compute, as in
exercise 1.37.

*Main> tan 1
1.5574077246549023
*Main> tanCF 1 100
1.557407724654902
-}

module Main where

contFrac n d k = f 1
    where f i = if i <= k then n i / (d i + f (i + 1))
                          else 0

tanCF x = contFrac (\i -> if i == 1 then x
                                    else -(x^2))
                   (\i -> i * 2 - 1)
