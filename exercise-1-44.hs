{-
Exercise 1.44.  The idea of smoothing a function is an important concept in sig-
nal processing. If f is a function and dx is some small number, then the smooth-
ed version of f is the function whose value at a point x is the average of f(x -
dx), f(x), and f(x + dx). Write a procedure smooth that takes as input a proced-
ure that computes f and returns a procedure that computes the smoothed f. It is
sometimes valuable to repeatedly smooth a function (that is, smooth the smoothed
function, and so on) to obtained the n-fold smoothed function. Show how to gene-
rate the n-fold smoothed function of any given function using smooth and repeat-
ed from exercise 1.43.
-}

module Main where

smooth :: (Fractional a) => (a -> a) -> (a -> a)
smooth f = \x -> (f (x - dx) + f x + f (x + dx)) / 3
    where dx = 0.0001

nFoldSmooth :: (Fractional a) => (a -> a) -> Int -> (a -> a)
nFoldSmooth f n = (repeated smooth n) f

----

repeated :: (a -> a) -> Int -> (a -> a)
repeated f n = \x -> iterate f x !! n

square x = x * x
