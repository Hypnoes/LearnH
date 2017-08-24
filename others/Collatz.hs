module Collatz (
    chain,
    numLongChains
    ) where

{- chain -}
chain :: Integer -> [Integer]
chain 1 = [1]
chain n 
    | even n = n:chain (n 'div' 2)
    | odd  n = n:chain (n*3 + 1)

{- number length chain -}
numLongChains :: Int 
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15
