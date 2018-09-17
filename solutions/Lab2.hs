
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

    Juffie <3

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

-- if ys is an element of the permutations of xs then this implies that
-- our function isPermutation must also be True
hoareTest :: Eq a => [a] -> [a] -> Bool
hoareTest xs ys = (elem ys (permutations xs)) --> isPermutation xs ys


{-
    TODO: TESTREPORT

    below example
-}

assignment4Report = do
    quickCheck (testPermutations :: [Int] -> Bool)
    print (hoareTest [1,2,3] [3,2,1])
    print (hoareTest [1,2,3] [3,3,3])


-- 5 Recognizing and generating derangements

-- Create derangements
derangements :: [Integer] -> [[Integer]]
derangements xs = [x | x <- permutations xs, checkAllIndices x xs]

-- Check if a given (valid) permutation is a derangement
checkAllIndices :: [Integer] -> [Integer] -> Bool
checkAllIndices [] [] = True
checkAllIndices [] _ = False
checkAllIndices _ [] = False
checkAllIndices (x:xs) (y:ys) | x == y = False
                              | otherwise = checkAllIndices xs ys

-- Checks is orig is a derangement of derag
isDerangement :: [Integer] -> [Integer] -> Bool
isDerangement derag orig = elem derag (derangements orig)

-- Throw all perms out where one of it's indices matches the original
deran :: Integer -> [[Integer]]
deran n = [x | x <- permutations [0..n-1],checkAllIndices x [0..n-1]]


{-
    TODO: TESTREPORT

    below example
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

assignment5Report = do
    print deranTest


-- 6 Implementing and testing ROT13 encoding


rot13 :: String -> String
rot13 [] = []
rot13 (x:xs) | x >= 'a' && x < 'n' ||
               x >= 'A' && x < 'N' = chr (ord x + 13) : rot13 xs
             | x >= 'n' && x <= 'z' ||
               x >= 'N' && x <= 'Z' = chr (ord x - 13) : rot13 xs
             | otherwise = x : rot13 xs
{-
    TODO: Explain what we do
-}

smallAlphabet = "abcdefghijklmnopqrstuvwxyz1234567890"
largeAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"

rotatedSmall = "nopqrstuvwxyzabcdefghijklm1234567890"
rotatedLarge = "NOPQRSTUVWXYZABCDEFGHIJKLM1234567890"

rot13LengthTest :: String -> Bool
rot13LengthTest xs = length xs == length (rot13 xs)

rot13Test :: [Char] -> Bool
rot13Test xs = ((length cs) > 0) && isAlpha (head cs) -->  (rot13 cs) /= cs && (rot13 (rot13 cs)) == cs
                where cs = show xs

assignment6Report = do
    print (rot13 smallAlphabet == rotatedSmall)
    print (rot13 largeAlphabet == rotatedLarge)
    print (rot13 (rot13 smallAlphabet) == smallAlphabet)
    print (rot13( rot13 largeAlphabet) == largeAlphabet)
    quickCheck rot13LengthTest
    quickCheck rot13Test

-- 7 Implementing and testing IBAN validation

--move 4 first characters to end of string
moveF4ToBack :: [Char] -> [Char]
moveF4ToBack xs = (drop 4 noSpaces) ++ (take 4 noSpaces)
                where noSpaces = (filter (/=' ') xs)

--replace all letters with numbers a=10,b=11,z=35
lettersToIntegers :: [Char] -> [Char]
lettersToIntegers [] = []
lettersToIntegers (c:cs) = if isUpper c then 
                                show((ord c - 65) + 10) ++ (lettersToIntegers cs)
                            else if (isLower c) then 
                                show((ord c - 97) + 10) ++ (lettersToIntegers cs)
                            else ([c] ++ (lettersToIntegers cs))

iban :: String -> Bool
iban cs = ((read (lettersToIntegers (moveF4ToBack cs)) :: Integer) `mod` 97) == 1


{-
    TODO: Meer correcte testcases handmatig toevoegen
    You should invent a way to test with incorrect examples also.

    Can you automate the test process?
    Yes you can generate correct and incorrect iban numbers:
    https://en.wikipedia.org/wiki/International_Bank_Account_Number 

    Incorrect testing: 
    You can increment one ore more numbers with one, since it is a mod 97 
    it will always fail.
-}
assignment7Report = do
    print (iban "GB82 WEST 1234 5698 7654 32")
    print (iban "FR76 3000 6000 0112 3456 7890 189")

main = do
    assignment1 <- redCurryTest
    print "Assignment 1 red curry"
    print assignment1
    print "Assignment 2 Triangles"
    print "Asssignment 3 properties strength"
    print propertieStrength
    print "exercise 4"
    assignment4Report
    print "exercise 5"
    assignment5Report
    print "exercise 6"
    assignment6Report
    print "exercise 7"
    assignment7Report