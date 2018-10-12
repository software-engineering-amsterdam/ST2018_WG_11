module Lab6 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture6

-- 1. Time: ... begonnen 11:25

-- https://stackoverflow.com/questions/9166148/how-to-implement-decimal-to-binary-function-in-haskell
int2Bin :: Integer -> [Integer]
int2Bin 0 = [0]
int2Bin n = reverse (helper n)

-- helper :: (Integral a1, Num a2) => a1 -> [a2]
helper 0 = []
helper n | n `mod` 2 == 1 = 1 : helper (n `div` 2)
         | n `mod` 2 == 0 = 0 : helper (n `div` 2)

-- starting n = 1, highest 2^k value, base and mod m as input
-- gives back list with potential mod values
modList :: Integer -> Integer -> Integer -> Integer -> [Integer]
modList n max b m = if n <= max then [b] ++ (modList (n * 2) max calc m) else [b]
    where calc = (b * b) `mod` m

-- second integer as base counter, should be 0
indexList :: [Integer] -> Int -> [Int]
indexList [] _ = []
indexList (x:xn) c = if x == 1 then c : (indexList xn (c + 1)) else indexList xn (c + 1)

exM' :: Integer -> Integer -> Integer -> Integer
exM' a b c = product [mods !! x | x <- indxs] `mod` c
    where indxs = indexList (reverse (int2Bin b)) 0
          mods = modList 1 (2 ^ (maximum indxs)) a c

correctTest b e m = m == 0 || e < 0 || exM b e m == exM' b e m
-- FAILED NOG.. 1 0 -2

-- 2. Time: ...
    -- Check that your implementation is more efficient than expM 
    -- by running a number of relevant tests and documenting the results.

-- 3. A function that generates the infinite list of composite natural numbers. Time: ...
-- composites :: [Integer]

-- 4. Time: ...
    -- Use the list of composite numbers to test Fermat's primality check. 
    -- What is the least composite number that you can find that fools the check, 
    -- for  prime_tests_F k with k=1,2,3 ? What happens if you increase k?

-- 5. Time: ...