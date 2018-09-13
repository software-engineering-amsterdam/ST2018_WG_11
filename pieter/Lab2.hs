
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

-- exercise 1 (~30 min)
-- make function to check range
isBetween :: Float -> Float -> Float -> Bool
isBetween x left right = (x >= left && x < right)

-- generate numbers and check what range they are in
numbers :: Int -> IO (Int, Int, Int, Int)
numbers = \n -> do list <- (probs n)
                   return ((length [ x | x <- list, isBetween x 0 0.25]),
                           (length [ x | x <- list, isBetween x 0.25 0.5]),
                           (length [ x | x <- list, isBetween x 0.5 0.75]),
                           (length [ x | x <- list, isBetween x 0.75 1]))

-- Function to easily print table
printNumbers = \n -> do num <- (numbers n)
                        print num


-- exercise 2 (started 10.50 stopped 12.15)
data Shape = NoTriangle | Equilateral 
            | Isosceles  | Rectangular | Other deriving (Eq,Show)               

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | not (((x + y) > z) && ((x + z) > y) && ((z + y) > x)) = NoTriangle
               | (x^2 + y^2 == z^2) || (x^2 + z^2 == y^2) || (z^2 + y^2 == x^2) = Rectangular
               | (x == y) && (y == z) = Equilateral
               | (x == y) || (x == z) || (y == z) = Isosceles
               | otherwise = Other

randomTriangle :: IO (Integer, Integer, Integer)
randomTriangle = do
                    x<-randomRIO (1, 10)
                    y<-randomRIO (1, 10)
                    z<-randomRIO (1, 10)
                    return (x,y,z)

-- randomTriangleShape :: Shape -> IO (Integer, Integer, Integer)
-- randomTriangleShape shape = do
--                                 x<-randomRIO (1, 10)
--                                 y<-randomRIO (1, 10)
--                                 return (x,y, y)

-- generateEquilateral :: IO (Integer, Integer, Integer)
-- generateEquilateral = do x <- randomIO
--                          return (x,x,x)

testTriangle :: IO Shape
testTriangle = do (x,y,z) <- randomTriangle
                  return (triangle x y z)


-- exercise 3 (100 min)
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)

weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
weaker   xs p q = stronger xs q p 


--domain -10 <= x <= 10
one, two, three :: Int -> Bool
one = (\ x -> even x && x > 3)
two = (\ x -> even x || x > 3)
three = (\ x -> (even x && x > 3) || even x)
-- four = (\ x -> (even x && x > 3) || even x)

functions :: [(Int -> Bool)]
functions = [one,two,three,even]

-- sort functions by strongest first
sortTest :: [(Int -> Bool)] -> [(Int -> Bool)] -> [(Int -> Bool)]
sortTest xs [] = xs
sortTest [] (y:ys) = sortTest [y] ys          
sortTest (x:xs) (y:ys) | stronger [-10..10] y x = sortTest (y:x:xs) ys
                       | otherwise = [x] ++ sortTest xs (y:ys)

-- quick check just for testing the lengths
test_sort = [ length[y | y <- map f [-10..10],y==False ]| f <- sortTest [] functions]
test2_sort = [ length[y | y <- map f [-10..10],y==True ]| f <- sortTest [] functions]
--currently no way of printing without ugly tuples


-- exercise 4 (100 mins)
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

-- exercise 5
-- Throw all perms out where one of it's indices matches the original
deran :: Integer -> [[Integer]]
deran n = [x | x <- permutations [0..n-1],checkAllIndices x [0..n-1]]

derangements :: [Integer] -> [[Integer]]
derangements xs = [x | x <- permutations xs,checkAllIndices x xs]

-- Check if any of the indices match
checkAllIndices :: [Integer] -> [Integer] -> Bool
checkAllIndices [] [] = True
checkAllIndices [] _ = False
checkAllIndices _ [] = False
checkAllIndices (x:xs) (y:ys) | x == y = False
                              | otherwise = checkAllIndices xs ys

isDerangement :: [Integer] -> [Integer] -> Bool
isDerangement derag orig = elem derag (derangements orig)

--TODO tests and test properties

-- exercise 6
-- ROT13 rotates the letters by 13 places in the alphabet
-- It is a special case of the ceaser cipher
-- The encreption and the decription method is the same
smallAlphabet = "abcdefghijklmnopqrstuvwxyz1234567890"
largeAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"


-- Translate letter to numbers add 13 and change them back
rot13 :: [Char] -> [Char]
rot13 cs = [if isUpper c then 
                chr ((((ord c - 65) + 13) `mod` 26) + 65) 
            else if (isLower c) then 
                chr ((((ord c - 97) + 13) `mod` 26) + 97) 
            else c | c <- cs]

-- TODO quickcheck properties

-- exercise 7
-- TODO still needs to check for length of iban string (think of spaces)
iban :: String -> Bool
iban cs = ((read (lettersToIntegers (moveF4ToBack cs)) :: Integer) `mod` 97) == 1

--check length
--move 4 first characters to end of string
--replace all letters with numbers a=10,b=11,z=35

moveF4ToBack :: [Char] -> [Char]
moveF4ToBack xs = (drop 4 noSpaces) ++ (take 4 noSpaces)
                where noSpaces = (filter (/=' ') xs)

lettersToIntegers :: [Char] -> [Char]
lettersToIntegers [] = []
lettersToIntegers (c:cs) = if isUpper c then 
                                show((ord c - 65) + 10) ++ (lettersToIntegers cs)
                            else if (isLower c) then 
                                show((ord c - 97) + 10) ++ (lettersToIntegers cs)
                            else ([c] ++ (lettersToIntegers cs))

-- TODO write tests for good and bad examples

main = do
    print "exercise 1"
    printNumbers 10000
    print "exercise 2"
    -- print (triangle 3 4 5)
    -- print (triangle 3 3 3)
    -- print (triangle 3 3 1)
    -- print (triangle 5 4 3)
    -- print (triangle 1 1 3)
    print "exercise 3"
    print ("n filtered out")
    print (test_sort)
    print ("n not filtered out")
    print (test2_sort)
    print "exercise 4"
    quickCheck (testPermutations :: [Int] -> Bool)
    print "exercise 5"
    print (permutations [0,1,2])
    print (deran 3)
    print (isDerangement [1,0,2] [0,1,2])
    print "exercise 6"
    print ([ord x | x <- smallAlphabet])
    print ([ord x | x <- largeAlphabet])
    print (rot13( rot13 smallAlphabet))
    print (rot13( rot13 largeAlphabet))
    print "exercise 7"
    print (iban "GB82 WEST 1234 5698 7654 32")

    

