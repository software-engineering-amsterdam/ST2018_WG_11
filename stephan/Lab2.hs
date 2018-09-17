
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

randomTriangle :: IO (Integer, Integer, Integer)
randomTriangle = do 
                   x <- randomRIO (1, 20)
                   y <- randomRIO (1, 20)
                   z <- randomRIO (x + y, 20 + x + y)
                --    z <- randomRIO (1,xs x + y - 90)
                   return (x, y, z)

testNoTrianlge :: Integer -> IO Bool
testNoTrianlge 0 = return True
testNoTrianlge n = do
                    (x,y,z) <- randomTriangle
                    p <- testNoTrianlge (n - 1)
                --     print (x, y, z)
                --     print (triangles x y z)
                    return ((triangles x y z /= NoTriangle) && p)

{-
    todo
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

remElem :: Eq a => a -> [a] -> [a]
remElem x [] = []
remElem x (y:ys) | x == y = ys
                 | otherwise = y : remElem x ys

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] ys = ys == []
isPermutation (x:xs) ys = isPermutation xs (remElem x ys)

testPermutation :: [Integer] -> Bool
testPermutation xs = (length xs < 5) --> foldr (&&) True 
                    [isPermutation xs ys | ys <- permutations xs]

{-
    Can you automate the test process? Use the techniques 
    presented in this week's lecture. Also use QuickCheck.

    Deliverables: Haskell program, concise test report, 
    indication of time spent.
-}

-- 5 Recognizing and generating derangements

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement [] x = False
isDerangement x [] = False
isDerangement (x:xs) (y:ys) = x /= y && isDerangement xs ys
{-
    Doesn't test if elements are double in the list or exist at all
-}

deran :: Integer ->  [[Integer]]
deran n = [x| x <- (permutations [0..(n-1)]), 
                isDerangement [0..(n-1)] x]
{-
    Next, define some testable properties for the 
    isDerangement function, and use some well-chosen 
    integer lists to test isDerangement.
-}

{-
    src: https://en.wikipedia.org/wiki/Derangement#Computational_complexity 
    Starting with n = 0, the numbers of derangements of n are:

    1, 0, 1, 2, 9, 44, 265, 1854, 14833, 133496, 1334961, 
    14684570, 176214841, 2290792932, ... 
-}

deranTest :: Bool
deranTest = length (deran 4) == 9 && length (deran 5) == 44 && 
            length (deran 6) == 265 && length (deran 7) == 1854 &&
            length (deran 8) == 14833

-- 6 Implementing and testing ROT13 encoding

{-
    First, give a specification of ROT13.

    ROT13 is a simple algorithm that splits the alphabet in two
    and then cast a tot n, b to o, etc
-}

{-
    Next, give a simple implementation of ROT13.
-}

rot13 :: String -> String
rot13 [] = []
rot13 (x:xs) | x >= 'a' && x < 'n' ||
               x >= 'A' && x < 'N' = chr (ord x + 13) : rot13 xs
             | x >= 'n' && x <= 'z' ||
               x >= 'N' && x <= 'Z' = chr (ord x - 13) : rot13 xs
             | otherwise = x : rot13 xs
{-
    Finally, turn the specification into a series of QuickCheck
    testable properties, and use these to test your implementation.
-}
rot13LengthTest :: String -> Bool
rot13LengthTest xs = length xs == length (rot13 xs)

-- 7 Implementing and testing IBAN validation
move :: Integer -> [a] -> [a]
move 0 xs = xs
move n (x:xs) = move (n - 1) xs ++ [x]

todigit :: Char -> [Integer]
todigit x = [y `div` 10, y `mod` 10] 
                where y = toInteger (ord x - ord 'a' + 10)

makeBigInt :: [Integer] -> Integer
makeBigInt [] = 0
makeBigInt (x:xs) = x + makeBigInt (map (*10) xs)

iban xs = makeBigInt (reverse (concat 
            [if isAlpha x then todigit x 
            else [toInteger (digitToInt x)] 
            | x <- (move 4 xs)])) 
                `mod` 97


main = do
        assignment1 <- redCurryTest
        print "Assignment 1 red curry"
        print assignment1
        print "Assignment 2 Triangles"
        print "Testing No Triangles"
        assignment2 <- (testNoTrianlge 100)
        print assignment2
        print "Asssignment 3 properties strength"
        print propertieStrength
        print "Assignment 4 Permutatations"
        quickCheck testPermutation
        print "Assignment 5 Derangments"
        print deranTest
        print "Assignment 6 rot13"
        print (rot13 "abcdefghijklmnopqrstuvwxyz")
        print (rot13 "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        quickCheck rot13LengthTest