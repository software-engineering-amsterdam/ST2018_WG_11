
-- NOTEREN HOEVEEL TIJD ALLES HEEFT GEKOST!


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
testsumsquare n
    | n < 0 = sumsquare (-n) == sumsquarefunc (-n)
    | otherwise = sumsquare n == sumsquarefunc n
    -- can ook abs gebruiken!

-- testsumsquare' :: Integer -> Bool
-- testsumsquare' n = n > 0 --> sumsquare n == sumsquarefunc n
    
sumtriple :: Integer -> Integer
sumtriple 0 = 0
sumtriple n = n ^ 3 + sumtriple (n - 1)

sumtriplefunc :: Integer -> Integer
sumtriplefunc n = (div (n * (n + 1)) 2) ^ 2

testsumtriple :: Integer -> Bool
testsumtriple n
    | n < 0 = sumtriple (-n) == sumtriplefunc (-n)
    | otherwise = sumtriple n == sumtriplefunc n
    -- kan abs gebruiken!
    -- of infix --> uitzoeken

-- 2. Redo exercise 4 of Workshop 1 by replacing sets by lists, and testing the property for integer lists of the form [1..n]

testpowerset :: [Integer] -> Bool
testpowerset xs = (length xs < 25) --> (length (subsequences xs) == 2 ^ (length xs))

-- Is the property hard to test? If you find that it is, can you given a reason why?
-- Yes very much! The lists get long very quick (...)

{- Give your thoughts on the following issue:
    when you perform the test for exercise 4, what are you testing actually?
    Are you checking a mathematical fact? Or are you testing whether subsequences
    satisfies a part of its specification? Or are you testing something else still?
    (...) -}


-- 3. Redo exercise 5 of Workshop 1 by replacing sets by lists, and testing the property for integer lists of the form [1..n]

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

testfactorial :: [Int] -> Bool
testfactorial xn = (length xn < 10) --> factorial (length xn) == length (permutations xn)

-- Is the property hard to test? If you find that it is, can you given a reason why?

{- Again, give your thoughts on the following issue:
    when you perform the test for exercise 5,
    what are you testing actually? Are you checking a mathematical fact?
    Or are you testing whether perms satisfies a part of its specification?
    Or are you testing something else still? -}


-- 4. The natural number 13 has the property that it is prime and its reversal, the number 31, is also prime. Write a function that finds all primes < 10000 with this property.

primerev :: [Integer]
-- first find primes < 1000
primesUntil = [x | x <- takeWhile (< 1000) primes]
primerev = [x | x <- primesUntil, prime (reversal x)]

-- How would you test this function, by the way?
-- (...)

-- 5. Find the smallest prime number that is a sum of 101 consecutive primes

primesoffset :: Int -> [Integer]
primesoffset n = take 101 (drop n primes)

sumprimes :: Int -> Integer
sumprimes n =
    if prime (sum (primesoffset n)) then sum(primesoffset n)
    else sumprimes (n + 1)

-- start with offset 0 
-- everytime not found; raise offset with one
-- when found; stop and return prime

-- 6. What is the smallest counterexample?
outputfunc :: Integer
outputfunc = head [product (take x primes) + 1 | x <- [1..], not(prime (product (take x primes) + 1))]

-- 7. Implement and test the Luhn Algorithm

-- 8. Crime Scene Investigation. Time: +/- 60 minutes first (wrong) solution, +/- 60 min new solution
xor :: Bool -> Bool -> Bool
xor x y | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False

accuses :: Boy -> Boy -> Bool
accuses Matthew Carl = False
accuses Matthew Matthew = False
accuses Matthew _ = True

accuses Peter Matthew = True
accuses Peter Jack = True

-- Jack says Peter and Matthew are lying
accuses Jack x = not (accuses Peter x) && not (accuses Matthew x)

-- Arnold says Matthew or Peter is speaking the truth
accuses Arnold x = xor (accuses Matthew x && not (accuses Peter x)) (not (accuses Matthew x) && accuses Peter x)

-- Carl says Arnold is lying
accuses Carl x = not (accuses Arnold x)

-- accuses Jack Carl = True
-- accuses Arnold Matthew = True
-- accuses Arnold Jack = True
-- accuses Carl Jack = True
-- accuses Carl Carl = True

accuses _ _ = False

-- list of every 
accusers :: Boy -> [Boy]
accusers x = [y | y <-boys, accuses y x]

guilty, honest :: [Boy]
-- check al lists of accusers, select the one with 3, as 3 people speak the truth
honest = head (filter (\x -> length x == 3) [accusers x | x<-boys])

guilty = [x| x<-boys, length (accusers x) == 3]

-- kan ook op andere manier; honest mensen bepalen door middel van de guilty!