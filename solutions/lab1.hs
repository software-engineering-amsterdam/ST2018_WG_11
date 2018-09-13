module Lab1 where
import Data.List
import Test.QuickCheck
import Data.Char

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
testsumsquare x = (x > 0) --> sumsquare x == sumsquarefunc x

sumtriple :: Integer -> Integer
sumtriple 0 = 0
sumtriple n = n ^ 3 + sumtriple (n - 1)

sumtriplefunc :: Integer -> Integer
sumtriplefunc n = (div (n * (n + 1)) 2) ^ 2

testsumtriple :: Integer -> Bool
testsumtriple x = (x > 0) --> sumtriple x == sumtriplefunc x


-- 2. Redo exercise 4 of Workshop 1 by replacing sets by lists, and testing the property for integer lists of the form [1..n]

powersetTest :: [Integer] -> Bool
powersetTest xs = (length xs < 25) --> 2 ^ (length xs) == 
                    length (subsequences xs)

{- Is the property hard to test? If you find that it is, can you given a reason why?
 Yes very much! The lists get long very quick (...) 
 thats why it gets very expensive to calculate
-}


{- Give your thoughts on the following issue:
    when you perform the test for exercise 4, what are you testing actually?
    Are you checking a mathematical fact? Or are you testing whether subsequences
    satisfies a part of its specification? Or are you testing something else still?
    (...) 
    What we are testing is given a random input and test 
    wether the length is correct with what we would expect.

    This is most definitly NOT a mathematical proof. This sample testings.
    -}

-- 3. Redo exercise 5 of Workshop 1 by replacing sets by lists, and testing the property for integer lists of the form [1..n]

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

testfactorial :: [Int] -> Bool
testfactorial xn = (length xn < 10) --> factorial (length xn) == length (permutations xn)

{- Is the property hard to test? 
   If you find that it is, can you given a reason why?

   Yes very much! The lists get long very quick (...) 
   thats why it gets very expensive to calculate
   
    Again, give your thoughts on the following issue: when 
    you perform the test for exercise 5, what are you testing
    actually? Are you checking a mathematical fact? Or are 
    you testing whether perms satisfies a part of its 
    specification? Or are you testing something else still? 

    What we are testing is given a random input and test 
    wether the length is correct with what we would expect.

    This is most definitly NOT a mathematical proof. This sample testings.
    -}

-- 4. The natural number 13 has the property that it is prime and its reversal, 
--    the number 31, is also prime. Write a function that finds all primes < 10000 with this property.

reversedPrimes :: [Integer]
reversedPrimes = let xs = takeWhile (< 10000) primes 
                  in [x |x <- xs, prime (reversal x)]
{-
    How would you test this function, by the way?
    This is very hard to test because by testing you would
    use the same functions which creates this list and thus
    be using the same errors. The best way would be to test
    if random correct answers are actualy in the list.
-}

-- 5. Find the smallest prime number that is a sum of 101 consecutive primes

generatePrimeFromList :: Integer
generatePrimeFromList = head ( filter prime 
                        [sum (take 101 (drop y primes)) | y <- [0..]])

testGeneratePrimeFromList :: Bool
testGeneratePrimeFromList = prime generatePrimeFromList
 
{-
    You can check wether the solutions is valid, but not if it 
    is the smallest valid solution.
-}

-- 6. What is the smallest counterexample?

counterConjecture :: [Integer]
counterConjecture = head [yprimes y | y <- [1..], 
                        not (prime (product (yprimes y) + 1))]
                    where yprimes = \y -> take y primes

-- 7. Implement and test the Luhn Algorithm
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f _ (x:[]) = [f x]
altMap f g (x:y:[]) = [f x,g y]
altMap f g (x:y:xs) = (f x) : (g y) : (altMap f g xs)

luhnDouble :: Integer -> Integer
luhnDouble x | x * 2 > 9 = (x * 2) - 9
             | otherwise = x * 2

intToList :: Integer -> [Integer]
intToList x = [toInteger( digitToInt y) | y <- (show x)]

reverseDouble :: [Integer] -> [Integer]
reverseDouble xs =  (altMap (\y -> y) (luhnDouble) (reverse xs))

luhn :: Integer -> Bool
luhn x = ((sum (reverseDouble (intToList x))) `mod` 10) == 0

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = n < 10 ^ 15 && n > 10 ^ 14 && 
          (n `div` 10 ^ 13 == 34 || n `div` 10 ^ 13 == 37) 
          && luhn n

isMaster n = n < 10 ^ 16 && n > 10 ^ 15 &&
            ((n `div` 10 ^ 12 >= 2221 && n `div` 10 ^ 12 <= 2720)
            || (n `div` 10 ^ 14 >= 51 && n `div` 10 ^ 14 <= 55))
            && luhn n

isVisa n = n < 10 ^ 16 && n > 10 ^ 15 &&
            n `div` 10 ^ 15 == 4
            && luhn n

-- list with valid credit card numbers
visaTestList :: [Integer]
visaTestList = [4485991127021869,4929364449072685,4024007117459502,4032239221220120,4929102118087224,4539202786465983,4916928283209267,4024007164631193,4024007118535946,4024007165567362]

masterTestList :: [Integer]
masterTestList = [5497755394834301,5140060650079555,5334274908193896,5285271107019228,5365762159957243,5172721726944319,5241214003235970,5546992705736344,5372641067552109,5221210400062987]

muricanTestList :: [Integer]
muricanTestList = [340533127037249,370225417418550,375365520275243,370070821153074,370502731004351,348500848068698,349266889993637,370522932316099,370530294267644,372123628957300]

-- visa tests
testVisaCorrect :: Integer -> Bool
testVisaCorrect y = isVisa(visaTestList !! (fromIntegral(y `mod` fromIntegral(length visaTestList))))

testVisaFalse :: Integer -> Bool
testVisaFalse y = not(isVisa((masterTestList ++ muricanTestList) !! (fromIntegral(y `mod` fromIntegral(length (masterTestList ++ muricanTestList))))))

-- mastercard tests
testMasterCorrect :: Integer -> Bool
testMasterCorrect y = isMaster(masterTestList !! (fromIntegral(y `mod` fromIntegral(length masterTestList))))

testMasterFalse :: Integer -> Bool
testMasterFalse y = not(isMaster((visaTestList ++ muricanTestList) !! (fromIntegral(y `mod` fromIntegral(length (visaTestList ++ muricanTestList))))))

-- american express tests
testMuricanCorrect :: Integer -> Bool
testMuricanCorrect y = isAmericanExpress(muricanTestList !! (fromIntegral(y `mod` fromIntegral(length muricanTestList))))

testMuricanFalse :: Integer -> Bool
testMuricanFalse y = not(isAmericanExpress((masterTestList ++ visaTestList) !! (fromIntegral(y `mod` fromIntegral(length (masterTestList ++ visaTestList))))))

-- Test all credit card functions
testAllCards = do
  quickCheck testVisaCorrect
  quickCheck testVisaFalse
  quickCheck testMasterCorrect
  quickCheck testMasterFalse
  quickCheck testMuricanCorrect
  quickCheck testMuricanFalse



-- 8. Crime Scene Investigation. Time: +/- 60 minutes first (wrong) solution, +/- 60 min new solution

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

accuses :: Boy -> Boy -> Bool
accuses Matthew Carl = False
accuses Matthew Matthew = False
accuses Matthew _ = True
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Peter _ = False
accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)
accuses Arnold x = xor (accuses Matthew x) (accuses Peter x) 
accuses Carl x = not (accuses Arnold x)


accusers :: Boy -> [Boy]
accusers x = [y| y <- boys, accuses y x]

-- 3 persons are honest so length must be 3
quilty, honest :: [Boy]
quilty = [x| x <- boys, length (accusers x) == 3]
honest = [y| x <- quilty, y <- (accusers x)]



main = do
    print "Assignment 1"
    quickCheck testsumsquare
    quickCheck testsumtriple
    print "Assignment 2 - powerset"
    quickCheck powersetTest
    print "Assignment 3 - test factorial"
    quickCheck testfactorial
    print "Assignment 4 - reversed primes"
    print reversedPrimes
    print "Assignment 5 - sum of 101 primes is a prime"
    print generatePrimeFromList
    print "Assignment 6 - counterexample"
    print counterConjecture
    print (product counterConjecture + 1)
    print "Assignment 7 - creditcards"
    testAllCards
    print "Assignment 8"
    print "Honest"
    print honest
    print "quilty"
    print quilty