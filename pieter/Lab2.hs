
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

sortTest :: [(Int -> Bool)] -> [(Int -> Bool)] -> [(Int -> Bool)]
sortTest xs [] = xs
sortTest [] (y:ys) = sortTest [y] ys          
sortTest (x:xs) (y:ys) | stronger [-10..10] y x = sortTest (y:x:xs) ys
                       | otherwise = [x] ++ sortTest xs (y:ys)

-- quick check just for testing the lengths
test = [ length[y | y <- map f [-10..10],y==False ]| f <- sortTest [] functions]
test2 = [ length[y | y <- map f [-10..10],y==True ]| f <- sortTest [] functions]


-- exercise 4 (started 14.00)
isPermutation :: Eq a => [a] -> [a] -> Bool

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
    print (test)
    print ("n not filtered out")
    print (test2)
    print "exercise 4"


    

