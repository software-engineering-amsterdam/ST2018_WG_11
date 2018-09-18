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
                      -- are in each quartile


-- 2. Recognizing triangles (10:45) - Time: 
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
    | x + y < z || y + z < x || x + z < y = NoTriangle
    | x == y && y == z = Equilateral
    | (x ^ 2) + (y ^ 2) == (z ^ 2) = Rectangular
    | x == y || x == z || y == z = Isosceles
    | otherwise = Other

-- randomTrio :: IO (Positive Integer, Positive Integer, Positive Integer)
-- randomTrio = do x <- randomRIO(0, 10)
--                 y <- randomRIO(0, 10)
--                 z <- randomRIO(0, 10)
--                 return (x, y, z)

-- checkTriangle:: IO Shape
-- checkTriangle = do trio <- randomTrio
--                    let (x, y, z) = trio
--                    return (triangle x y z)


-- Three different NoTriangle cases
testNoTriangle1 :: (Positive Integer) -> (Positive Integer) -> (Positive Integer) -> Bool
testNoTriangle1 (Positive x) (Positive y) (Positive z) = (x + y < z) --> triangle x y z == NoTriangle

testNoTriangle2 :: (Positive Integer) -> (Positive Integer) -> (Positive Integer) -> Bool
testNoTriangle2 (Positive x) (Positive y) (Positive z) = (y + z < x) --> triangle x y z == NoTriangle

testNoTriangle3 :: (Positive Integer) -> (Positive Integer) -> (Positive Integer) -> Bool
testNoTriangle3 (Positive x) (Positive y) (Positive z) = (x + z < y) --> triangle x y z == NoTriangle


testEquilateral :: (Positive Integer) -> Bool
testEquilateral (Positive x) = triangle x x x == Equilateral


-- Three different Isosceles cases: (x x y) (x y x) (y x x)
testIsosceles1 :: (Positive Integer) -> (Positive Integer) -> Bool
testIsosceles1 (Positive x) (Positive y) = (x /= y && 2 * x >= y) --> triangle x x y == Isosceles

testIsosceles2 :: (Positive Integer) -> (Positive Integer) -> Bool
testIsosceles2 (Positive x) (Positive y) = (x /= y && 2 * x >= y) --> triangle x y x == Isosceles

testIsosceles3 :: (Positive Integer) -> (Positive Integer) -> Bool
testIsosceles3 (Positive x) (Positive y) = (x /= y && 2 * x >= y) --> triangle y x x == Isosceles


testRectangular :: (Positive Integer) -> (Positive Integer) -> (Positive Integer) -> Bool
testRectangular (Positive x) (Positive y) (Positive z) = ((z ^ 2) + (y ^ 2) == (x ^ 2)) --> triangle x y z == Rectangular


-- A triangle with the properties (a + 1) (b + 2) (a + b + 2) create 'Other' triangles,
-- as they cannot be placed in the other cases, as long as a is the smallest number and
-- b is the greater number
testOther :: (Positive Integer) -> (Positive Integer) -> Bool
testOther (Positive x) (Positive y) = let a = minimum[x,y]
                                          b = maximum[x,y]
                                      in (triangle (a + 1) (b + 2) (a + b + 2) == Other)


{-
The tests use most of the same functions as the function. This means that the domain we're testing in
is already expected to be the same as the domain of the formula. This makes verifying hard(er) in this
case. It does test if it holds for the cases within that specific domain (via quickCheck). The test
cases are reduced because of the restrictions in quickCheck. 
 -}


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

-- usage; stronger [-10..10] test2 test3
--        weaker [-10..10] test2 test3

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

-- Next, define some testable properties for this function, and use a number of well-chosen lists to test isPermutation. You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure?

-- Provide an ordered list of properties by strength using the weakear and stronger definitions.

-- Can you automate the test process? Use the techniques presented in this week's lecture. Also use QuickCheck.

-- Deliverables: Haskell program, concise test report, indication of time spent.



-- 5. Recognizing and generating derangements

-- 6. Implementing and testing ROT13 encoding

-- 7. Implementing and testing IBAN validation

main = do
        print "Exercise 1"
        counting

        print "Exercise 2"
        print "Checking Equilateral Triangles"
        quickCheck testEquilateral

        print "Checking Three Isosceles Triangle Cases"
        print "case1: x == y"
        quickCheck testIsosceles1
        print "case2: x == z"
        quickCheck testIsosceles2
        print "case3: y == z"
        quickCheck testIsosceles3

        print "Checking Rectangular Triangles"
        quickCheck testRectangular

        print "Checking Other Triangles"
        quickCheck testOther

        print "Checking Three NoTriangle Cases"
        print "case1: x + y < z"
        quickCheck testNoTriangle1
        print "case1: y + z < x"
        quickCheck testNoTriangle2
        print "case1: x + z < y"
        quickCheck testNoTriangle3

