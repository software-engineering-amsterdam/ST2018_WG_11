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



-- 1. Redo exercises 2 and 3 of Workshop 1 by writing QuickCheck tests for these statements - Time: 45 mins

sumsquare :: Integer -> Integer
sumsquare 0 = 0
sumsquare n = n ^ 2 + sumsquare (n - 1)

sumsquarefunc :: Integer -> Integer
sumsquarefunc n = div (n * (n + 1)*(2*n + 1)) 6

testsumsquare :: Integer -> Bool
testsumsquare n
    | n < 0 = sumsquare (-n) == sumsquarefunc (-n)
    | otherwise = sumsquare n == sumsquarefunc n

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
    -- maybe use abs/infix!

-- 2. Redo exercise 4 of Workshop 1 by replacing sets by lists,
--    and testing the property for integer lists of the form [1..n] - Time: 45 mins

testpowerset :: [Integer] -> Bool
testpowerset xs = (length xs < 25) --> (length (subsequences xs) == 2 ^ (length xs))


-- 3. Redo exercise 5 of Workshop 1 by replacing sets by lists,
--    and testing the property for integer lists of the form [1..n] - Time: 30 mins

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

testfactorial :: [Int] -> Bool
testfactorial xn = (length xn < 10) --> factorial (length xn) == length (permutations xn)

-- 4. The natural number 13 has the property that it is prime and its reversal,the number 31, is also prime.
--    Write a function that finds all primes < 10000 with this property - Time: 20/30 mins

primerev :: [Integer]
-- first find primes < 1000
primesUntil = [x | x <- takeWhile (< 1000) primes]
primerev = [x | x <- primesUntil, prime (reversal x)]


-- 5. Find the smallest prime number that is a sum of 101 consecutive primes - Time: 60 mins

primesoffset :: Int -> [Integer]
primesoffset n = take 101 (drop n primes)

-- start with offset 0 
sumprimes :: Int -> Integer
sumprimes n =
    if prime (sum (primesoffset n)) then sum(primesoffset n)
    else sumprimes (n + 1)

-- 6. What is the smallest counterexample? - Time: 40/50 mins
outputfunc :: Integer
outputfunc = head [product (take x primes) + 1 | x <- [1..], not(prime (product (take x primes) + 1))]

-- 7. Implement and test the Luhn Algorithm - Time: dont know.. forgot to check!
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f _ (x:[]) = [f x]
altMap f g (x:y:[]) = [f x,g y]
altMap f g (x:y:xs) = (f x) : (g y) : (altMap f g xs)

luhnDouble :: Integer -> Integer
luhnDouble x | x * 2 > 9 = (x * 2) - 9
             | otherwise = x * 2

intToArray :: Integer -> [Integer]
intToArray 0 = []
intToArray n = n `mod` 10 : intToArray (n `div` 10)

reverseDouble :: [Integer] -> [Integer]
reverseDouble xs =  (altMap (\y -> y) (luhnDouble) (reverse xs))

luhnAl :: Integer -> Bool
luhnAl x = ((sum (reverseDouble (intToArray x))) `mod` 10) == 0

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = n < 10 ^ 15 && n > 10 ^ 14 && 
          (n `div` 10 ^ 13 == 34 || n `div` 10 ^ 13 == 37) 
          && luhnAl n

isMaster n = n < 10 ^ 16 && n > 10 ^ 15 &&
            ((n `div` 10 ^ 12 >= 2221 && n `div` 10 ^ 12 <= 2720)
            || (n `div` 10 ^ 14 >= 51 && n `div` 10 ^ 14 <= 55))
            && luhnAl n

isVisa n = n < 10 ^ 16 && n > 10 ^ 15 &&
            n `div` 10 ^ 15 == 4
            && luhnAl n

-- list with valid credit card numbers
visaTestList :: [Integer]
visaTestList = 
    [4485991127021869,4929364449072685,4024007117459502,
    4032239221220120,4929102118087224,4539202786465983,
    4916928283209267,4024007164631193,4024007118535946,
    4024007165567362]

masterTestList :: [Integer]
masterTestList = 
    [5497755394834301,5140060650079555,5334274908193896,
    5285271107019228,5365762159957243,5172721726944319,
    5241214003235970,5546992705736344,5372641067552109,
    5221210400062987]

amExprTestList :: [Integer]
amExprTestList = 
    [340533127037249,370225417418550,375365520275243,
    370070821153074,370502731004351,348500848068698,
    349266889993637,370522932316099,370530294267644,
    372123628957300]

-- visa tests
visaCorrect :: Integer -> Bool
visaCorrect y = 
    isVisa(visaTestList !! (fromIntegral(y `mod` fromIntegral(length visaTestList))))

visaFalse :: Integer -> Bool
visaFalse y = 
    not(isVisa((masterTestList ++ amExprTestList) !! (fromIntegral(y `mod` fromIntegral(length (masterTestList ++ amExprTestList))))))

-- mastercard tests
masterCorrect :: Integer -> Bool
masterCorrect y = 
    isMaster(masterTestList !! (fromIntegral(y `mod` fromIntegral(length masterTestList))))

masterFalse :: Integer -> Bool
masterFalse y = 
    not(isMaster((visaTestList ++ amExprTestList) !! (fromIntegral(y `mod` fromIntegral(length (visaTestList ++ amExprTestList))))))

-- american express tests
amExprCorrect :: Integer -> Bool
amExprCorrect y = 
    isAmericanExpress(amExprTestList !! (fromIntegral(y `mod` fromIntegral(length amExprTestList))))

amExprFalse :: Integer -> Bool
amExprFalse y = 
    not(isAmericanExpress((masterTestList ++ visaTestList) !! (fromIntegral(y `mod` fromIntegral(length (masterTestList ++ visaTestList))))))

-- 8. Crime Scene Investigation. Time: +/- 60 minutes first (wrong) solution, +/- 40 min new solution
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

accuses _ _ = False

accusers :: Boy -> [Boy]
accusers x = [y | y <-boys, accuses y x]

guilty, honest :: [Boy]
-- check al lists of accusers, select the one with 3, as 3 people speak the truth
honest = head (filter (\x -> length x == 3) [accusers x | x<-boys])
guilty = [x| x<-boys, length (accusers x) == 3]