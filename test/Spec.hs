import Data.Maybe
import Primes
import Test.QuickCheck

prop_validPrimesOnly val = if val < 2 || val > length primes
                            then result == Nothing
                            else isJust result
    where result = isPrime val

prop_primesArePrime val = if result == Just True
                            then length divisors == 0
                            else True
    where result = isPrime val
          divisors = filter ((==0) . (val `mod`)) [2 .. (val -1)]

prop_nonPrimesAreComposite val = if result == Just False
                                    then length divisors > 0
                                    else True
    where result = isPrime val
          divisors = filter ( (==0) . (val `mod`)) [2 .. (val -1)]

prop_factorsMakeOriginal val = if result == Nothing
                                then True
                                else product (fromJust result ) == val
    where result = primeFactors val

prop_allFactorsPrime val = if result == Nothing
                            then True
                            else all (== Just True) (map isPrime (fromJust result))
    where result = primeFactors val

main :: IO ()
main = do
    putStrLn "Value only for valid input"
    quickCheck prop_validPrimesOnly
    putStrLn "Are prime"
    quickCheckWith stdArgs { maxSuccess = 1000} prop_primesArePrime
    putStrLn "Negative cases are composite"
    quickCheckWith stdArgs { maxSuccess = 1000} prop_nonPrimesAreComposite
    putStrLn "Factors product to original"
    quickCheck prop_factorsMakeOriginal
    putStrLn "All factors are prime"
    quickCheck prop_allFactorsPrime
