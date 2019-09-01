import Data.Maybe
import Primes
import Test.QuickCheck

prop_validPrimesOnly val = if val <0 || val > length primes
                            then result == Nothing
                            else isJust result
    where result = isPrime val

main :: IO ()
main = do
    quickCheck prop_validPrimesOnly
