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

-- 1. Redo exercises 2 and 3 of Workshop 1 by writing QuickCheck tests for these statements

sumsquare :: Integer -> Integer
sumsquare 0 = 0
sumsquare n = n ^ 2 + sumsquare (n - 1)

sumsquarefunc :: Integer -> Integer
sumsquarefunc n = div (n * (n + 1)*(2*n + 1)) 6

testsumsquare :: Integer -> Bool
testsumsquare x = (x > 0) --> sumSquare x == func x


-- 2. Redo exercise 4 of Workshop 1 by replacing sets by lists, and testing the property for integer lists of the form [1..n]

-- 3. Redo exercise 5 of Workshop 1 by replacing sets by lists, and testing the property for integer lists of the form [1..n]

-- 4. The natural number 13 has the property that it is prime and its reversal, the number 31, is also prime. Write a function that finds all primes < 10000 with this property.

-- 5. Find the smallest prime number that is a sum of 101 consecutive primes

-- 6. What is the smallest counterexample?

-- 7. Implement and test the Luhn Algorithm

-- 8. Crime Scene Investigation. Time: +/- 60 minutes first (wrong) solution, +/- 60 min new solution
