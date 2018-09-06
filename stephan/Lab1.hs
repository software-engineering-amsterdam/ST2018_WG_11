
module Lab1 where
import Data.List
import Test.QuickCheck    

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- 1 TimeUsed: 45 minuts
func :: Integer -> Integer
func x = x * (x + 1) * (2 * x + 1) `div` 6

func2 :: Integer -> Integer
func2 x = (x * (x + 1) `div` 2) ^ 2

sumSquare :: Integer -> Integer
sumSquare 0 = 0
sumSquare n = n^2 + sumSquare (n - 1)

sumPower :: Integer -> Integer
sumPower 0 = 0
sumPower x = x^3 + sumPower (x - 1)

myTest :: Integer -> Bool
myTest x = (x > 0) --> sumSquare x == func x

myTest2 :: Integer -> Bool
myTest2 x = (x > 0) --> sumPower x == func2 x

-- 2 45 Minuts
powersetTest :: [Integer] -> Bool
powersetTest xs = (length xs < 25) --> 2 ^ (length xs) == 
                    length (subsequences xs)
-- Yes this is very hard to test. 
-- This is because it is very expensive to calculate
-- What we are testing is just by given a random input and test
--    wether the length is correct with what we would expect.
--    This is most definitly NOT a mathematical proof

-- 3 30 minuts
perms :: [Integer] -> Int
perms [] = 1
perms (x:xs) = length (x:xs) * perms xs

permsTest :: [Integer] -> Bool
permsTest xs = (length xs < 10) --> perms xs == 
                length (permutations xs)

-- 4 20 minuts
reversedPrimes :: [Integer]
reversedPrimes = let xs = takeWhile (< 10000) primes 
                  in [x |x <- xs, prime (reversal x)]

-- You could test this by checking if the reversed is                 
-- an element in the list

-- 5 70 minuts
sumPrimes :: [Integer] -> [Integer]
sumPrimes (x:xs) | prime (sum (take 101 (x:xs))) = take 101 (x:xs)
                 | otherwise = sumPrimes xs

-- You can check wether it is a prime, but not if it is the 
-- smallest prime
-- The number is 37447

-- 6 60 minuts
-- test :: Int -> [Integer]
-- test n | prime (product (take n primes) + 1) = test (n + 1)
--        | otherwise = take n primes


counterConjecture = head [take y primes | y <- [1..], 
                    not (prime (product (take y primes) + 1))]

-- 7 90 minuts

intToArray :: Integer -> [Integer]
intToArray 0 = []
intToArray n = n `mod` 10 : intToArray (n `div` 10)

myMap :: (Integer -> Integer) -> [Integer] -> [Integer]
myMap f [] = []
myMap f (x:y:xs) = x : f y : myMap f xs
myMap f (x:xs) = x : []

doubleSecond :: [Integer] -> [Integer]
doubleSecond xs = myMap (*2) xs

rem9 :: [Integer] -> [Integer]
rem9 xs = [if x > 9 then (x - 9) else x| x <- xs]

luhn :: Integer -> Bool
luhn n = sum (rem9 (doubleSecond (intToArray n))) `mod` 10 == 0

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = n < 10 ^ 15 && n > 10 ^ 14 && luhn n
isMaster n = n < 10 ^ 16 && n > 10 ^ 15 && luhn n
isVisa = isMaster

-- 8
accuses :: Boy -> Boy -> Bool
-- accuses Peter Matthew = True
-- accuses Peter Jack = True
accuses Jack Matthew = True
accuses Jack Peter = True
-- accuses Arnold Matthew = True
-- accuses Arnold Peter = True
accuses Carl Arnold = True
accuses _ _ = False

accusers :: Boy -> [Boy]
accusers Matthew = [Jack]
accusers Peter = [Jack]
accusers Jack = []
accusers Arnold = [Carl]
accusers Carl = []
-- accusers Matthew = [Peter, Jack, Arnold]
-- accusers Peter = [Jack, Arnold]
-- accusers Jack = [Peter]
-- accusers Arnold = [Carl]
-- accusers Carl = []



main = do
  print "Assignment 1"
  quickCheck myTest
  quickCheck myTest2
  print "Assignment 2 - powerset"s
  quickCheck permsTest
  print "Assignment 4 - reversed primes"
  print reversedPrimes
  print "Assignment 5 - sum of 101 primes is a prime"
  print (sumPrimes primes)
  print "Assignment 6 - counterexample"
  print counterConjecture