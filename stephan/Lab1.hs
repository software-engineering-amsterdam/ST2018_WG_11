
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
                  in [x |x <- xs, elem (reversal x) xs]

-- You could test this by checking if the reversed is                 
-- an element in the list

-- 5 40 minuts
sumPrimes :: Integer -> [Integer] -> Integer -> [Integer] ->
             [Integer]
sumPrimes amount xs 0 ys 
                 | (prime (sum ys)) = ys 
                 | otherwise = sumPrimes amount xs amount []
sumPrimes amount (x:xs) counter ys = 
                sumPrimes amount xs (counter - 1) (ys ++ [x])

-- Yes it is 163003
-- You can check it by prime (sum (sumPrimes 101 primes 0 []))

-- 6



main = do
  print "Assignment 1"
  quickCheck myTest
  quickCheck myTest2
  print "Assignment 2 - powerset"
  quickCheck powersetTest
  print "Assignment 3 - permutations"
  quickCheck permsTest
  print "Assignment 4 - reversed primes"
  print reversedPrimes
  print "Assignment 5 - sum of 101 primes is a prime"
  print (sumPrimes 101 primes 0 [])