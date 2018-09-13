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


-- 2. Recognizing triangles (10:45) - Time: 
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
    | x + y < z = NoTriangle
    | x == y && y == z = Equilateral
    | (x ^ 2) + (y ^ 2) == (z ^ 2) = Rectangular
    | x == y && x == z && y == z = Isosceles
    | otherwise = Other

randomTrio :: IO (Integer, Integer, Integer)
randomTrio = do x <- randomRIO(0, 10)
                y <- randomRIO(0, 10)
                z <- randomRIO(0, 10)
                return (x, y, z)

checkTriangle:: IO Shape
checkTriangle = do trio <- randomTrio
                   let (x, y, z) = trio
                   return (triangle x y z)

-- Test functions equilateral
randomTrioEqu :: IO (Integer, Integer, Integer)
randomTrioEqu = do x <- randomRIO(0, 100)
                   return (x, x, x)

checkTriangleEqu :: IO Bool
checkTriangleEqu = do trio <- randomTrioEqu
                      let (x, y, z) = trio
                      return ((triangle x y z) == Equilateral)

testEquilateral :: Int -> IO Bool
testEquilateral 0 = return True             
testEquilateral n = do test <- checkTriangleEqu
                       p <- testEquilateral (n - 1)
                       return (test && p)


-- 3. Testing properties strength
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\x -> p x --> q x)
weaker   xs p q = stronger xs q p 

-- part a
test1, test2, test3 :: Int -> Bool
test1 = \x -> even x && x > 3
test2 = \x -> even x || x > 3
test3 = \x -> (even x && x > 3) || even x

-- f1 = [x | x <- [-10..10], test1 x]
-- f2 = [x | x <- [-10..10], test2 x]
-- f3 = [x | x <- [-10..10], test3 x]
-- f4 = [x | x <- [-10..10], even x]

-- usage; stronger [-10..10] test2 test3
--        weaker [-10..10] test2 test3

-- sortStrength :: [Int -> Bool] -> [Int -> Bool]
-- sortStrength xn = [x | x <- xn, y <- xn]
-- sortStrength f1 f2 = if stronger [-10..10] f1 f2 then [test1, test2] else [test1]

-- part b
-- sortStrength :: [Int -> Bool] -> [Int -> Bool]
-- sortStrength [] = []
-- sortStrength [x] = [x]
-- sortStrength (x:y:xs) = if stronger [-10..10] x y then x:(sortStrength (y:xs)) else y:(sortStrength (x:xs))


-- QUICKSORT GEBRUIKEN?

-- quicksort [] = []
-- quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
--     where
--         lesser = filter (< p) xs
--         greater = filter (>= p) xs



-- 4. Recognizing Permutations
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs xt = elem xt (permutations xs)


-- 5. Recognizing and generating derangements

-- 6. Implementing and testing ROT13 encoding

-- 7. Implementing and testing IBAN validation

-- main = do
--         print "check"
--         counting