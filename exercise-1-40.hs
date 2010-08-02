{-
Exercise 1.40.  Define a procedure cubic that can be used together with the newtons-method procedure
in expressions of the form

(newtons-method (cubic a b c) 1)

to approximate zeros of the cubic x^3 + ax^2 + bx + c.

*Main> newtonsMethod (cubic 1 1 1) 1
-0.9999999999997795
*Main> (cubic 1 1 1) (-1)
0.0
-}

module Main where

cubic :: (Floating a) => a -> a -> a -> (a -> a)
cubic a b c = \x -> x ** 3 + a * x ** 2 + b * x + c

--------------------------------

deriv :: (Fractional a) => (a -> a) -> (a -> a)
deriv g = \x -> (g (x + dx) - g x) / dx
    where dx = 0.00001

fixedPoint :: (Fractional a, Ord a) => (a -> a) -> a -> a
fixedPoint f x = if closeEnough x (f x)
                         then f x
                         else fixedPoint f (f x)
    where closeEnough a b = abs (a - b) < tolerance
          tolerance = 0.00001

newtonsMethod :: (Fractional a, Ord a) => (a -> a) -> a -> a
newtonsMethod g guess = fixedPoint (newtonTransform g) guess
    where newtonTransform g = \x -> x - (g x / (deriv g) x)
