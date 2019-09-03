module Main where

import Primes

toResult :: Maybe Bool -> String
toResult (Just True) = "It' a prime!"
toResult (Just False) = "Not a prime"
toResult Nothing = "Not valid"

main :: IO ()
main = do
    putStrLn "Insert a number to check its primarity" 
    n <- getLine
    let result = (toResult . isPrime . read) n
    putStrLn result
    
