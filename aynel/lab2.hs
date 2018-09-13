module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)
    
data Shape = NoTriangle | Equilateral
            | Isosceles  | Rectangular | Other deriving (Eq,Show)


-- 1. Counting the numbers in the quartiles - Time: 30 mins
counting :: IO (Int, Int, Int, Int)
counting = do output <- probs 100000
              return (length [x | x <-output, x <= 0.25],
                      length [x | x <-output, x > 0.25, x <=0.5],
                      length [x | x <-output, x > 0.5, x <= 0.75],
                      length [x | x <-output, x > 0.75])

                      -- conclusion: with 10000 numbers 'roughly' 2500 of them
                      -- should are in each quartile


-- 2. Recognizing triangles
triangle :: Integer -> Integer -> Integer -> Shape


-- 3. Testing properties strength

-- 4. Recognizing Permutations

-- 5. Recognizing and generating derangements

-- 6. Implementing and testing ROT13 encoding

-- 7. Implementing and testing IBAN validation

-- main = do
--         print "check"
--         counting