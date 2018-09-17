
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

-- 1 Red Curry random number generator test

redCurryTest :: IO [Int]
redCurryTest = do list <- probs 10000
                  return ([length [x | x <- list, x > 0 && x < 0.25],
                          length [x | x <- list, x >= 0.25 && x < 0.50],
                          length [x | x <- list, x >= 0.50 && x < 0.75],
                          length [x | x <- list, x >= 0.75 && x < 1]])

{-
      Since it is an open interval; (0..1). I would expect the
      first and last list to contain less items.

      Running the test a few times I can see that it differs every
      time with around ~50 in eacht quartile. The random generator
      seems to be working fine. I do not pocces enough calculation 
      power to test my first hypothosis.
-}

-- 2 Recognizing triangles

triangles :: Integer -> Integer -> Integer -> Shape
triangles x y z | x <= 0 || y <= 0 || z <= 0 || (x + y > z) = NoTriangle
                | x == y && y == z = Equilateral
                | x ^ 2 + y ^ 2 == z ^ 2 ||
                  x ^ 2 + z ^ 2 == y ^ 2 ||
                  z ^ 2 + y ^ 2 == x ^ 2 = Rectangular
                | x == y || y == z || x == z = Isosceles
                | otherwise = Other

{-

    TODO: Create test cases for each triangle

-}


-- 3 Testing properties strength
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\x -> p x --> q x)
weaker   xs p q = stronger xs q p 

test1, test2, test3, test4 :: Int -> Bool
test1 = \x -> even x && x > 3
test2 = \x -> even x || x > 3
test3 = \x -> (even x && x > 3) || even x
test4 = even

testList = [(test1,"even x && x > 3"), (test2, "even x || x > 3"), (test3, "(even x && x > 3) || even x"), (test4,"even")]


sortTest [] = []
sortTest ((f,name):xs) = sortTest [(f1,n1) | (f1, n1) <- xs, stronger [-10..10] f1 f]
                            ++ [name] ++ 
                            sortTest [(f1,n1) | (f1, n1) <- xs, not (stronger [-10..10] f1 f)]

propertieStrength = sortTest testList


-- 4 Recognizing Permutations

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = ((length x) == 0) && ((length y) == 0)
                        where (x,y) = crossAway (xs, ys)

-- Cross away elements of each list against eachother. List should be empty in the end
crossAway :: Eq a => ([a], [a]) -> ([a], [a])
crossAway (xs, []) = (xs,[])
crossAway ([],ys) = ([],ys)
crossAway ((x:xs), ys) | (length ((delete x ys)) == (length ys)) = ([x],[])
                       | otherwise = crossAway (xs, (delete x ys))

-- generate a random array and test its permutations
-- becuase of the arbitrary type we have to specify the type of the input when using quickcheck
testPermutations :: Eq a => [a] -> Bool
testPermutations xs = (length xs < 8) --> all (\ys -> isPermutation ys xs) (permutations xs)
-- limit because the size of permutations can get very large

hoareTest :: (a -> Bool) -> (a -> a) -> (a -> Bool) -> [a] -> Bool
hoareTest precondition f postcondition = all (\x -> precondition x --> postcondition (f x))

-- precondition = \x y -> isPermutation y x

-- hoareTest original perm = isPermutation original perm --> elem perm (permutations original)
-- domein = [[x | x <- [0..y]]| y <- [0..10]--
--hoareTest precondition permutations postcondition [0..100]

hoareTest isPermutation permutations elem [([1,2,3], [3,2,1])]



main = do
    assignment1 <- redCurryTest
    print "Assignment 1 red curry"
    print assignment1
    print "Assignment 2 Triangles"
    print "Asssignment 3 properties strength"
    print propertieStrength
    print "exercise 4"
    quickCheck (testPermutations :: [Int] -> Bool)
