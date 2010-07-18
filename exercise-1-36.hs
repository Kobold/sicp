{-
Exercise 1.36.  Modify fixed-point so that it prints the sequence of approximations it generates,
using the newline and display primitives shown in exercise 1.22. Then find a solution to x^x = 1000
by finding a fixed point of x -> log(1000)/log(x). (Use Scheme's primitive log procedure, which
computes natural logarithms.) Compare the number of steps this takes with and without average
damping. (Note that you cannot start fixed-point with a guess of 1, as this would cause division
by log(1) = 0.)

With damping: 8 steps
With damping: 24 steps

-}

module Main where

tolerance = 0.001

averageDamp :: (Fractional a) => (a -> a) -> (a -> a)
averageDamp f = \x -> average x (f x)
    where average a b = (a + b) / 2

fixedPoint :: (Double -> Double) -> Double -> [Double]
fixedPoint f x = if closeEnough x (f x)
                         then [x, f x]
                         else x : fixedPoint f (f x)
    where closeEnough a b = abs (a - b) < tolerance

main = do display withDamping
          putStrLn ""
          display withoutDamping
    where f x = log 1000 / log x
          withDamping = fixedPoint (averageDamp f) 2
          withoutDamping = fixedPoint f 2
          display x = do putStrLn ("With damping: " ++ show (length x) ++ " steps")
                         mapM_ putStrLn (map show x)
