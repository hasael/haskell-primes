module Main where

import Primes


toResult :: Maybe Bool -> String
toResult (Just True) = "It' a prime!"
toResult (Just False) = "Not a prime"
toResult Nothing = "Not valid"

factorNumbers :: Int -> String
factorNumbers n = case result of
            Nothing -> ""
            Just (x:[]) ->  ""
            Just (x:xs) -> "Factorials are " ++ concat( map ((++",") . show) (x:xs))
            
    where result = primeFactors n

main :: IO ()
main = do
    putStrLn "Insert a number to check its primarity" 
    n <- getLine
    let value = read n
    let result = (toResult . isPrime ) value
    putStrLn result
    putStrLn (factorNumbers value)
    
