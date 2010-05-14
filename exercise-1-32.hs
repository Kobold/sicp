{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where
import Prelude hiding (sum, product)

-- a. recursive version of accumulate
accumulateR combiner nullValue term a next b =
    if a > b then nullValue
             else combiner (term a)
                           (accumulateR combiner
                                        nullValue
                                        term
                                        (next a)
                                        next
                                        b)

-- or more simply
accumulateS combiner nullValue term a next b = f a
    where f a = if a > b then nullValue
                         else combiner (term a) (f $ next a)

sum     = accumulateR (+) 0
product = accumulateI (*) 1

-- b. iterative version of accumulate
accumulateI combiner nullValue term a next b = iter a nullValue
    where iter a result =
              if a > b then result
                       else iter (next a)
                                 (combiner (term a) result)

main = do putStr "sum id 1 (+1) 10 = "
          putStrLn (show $ sum id 1 (+1) 10)
          putStr "product id 1 (+1) 5 = "
          putStrLn (show $ product id 1 (+1) 5)
