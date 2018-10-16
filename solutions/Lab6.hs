module Lab6 where 

import Data.List
import Lecture6
import Test.QuickCheck
import System.Random

-- 1 & 2 Time: 3.5 hours

-- line 220 lecture 6

{-
    quickCheck function to test correctness
-}
modulairCheck :: Integer -> Integer -> Integer -> Bool
modulairCheck b e m = e < 0 || m == 0 || (b^(e) `mod` m == (exM b (e) m))

{-
    Old method from lecture to compare running time
-}
exM' :: Integer -> Integer -> Integer -> Integer
exM' = expM

test1_slow = exM' 5 99999999 19
test1_fast = exM 5 99999999 19
test2_slow = exM' 7 999999999 17
test2_fast = exM 7 999999999 17

assignment1 = do
    print "---- Assignment 1 & 2"
    print "Testing runtime of different functions and correctness"
    quickCheck (withMaxSuccess 10000 modulairCheck)
    print ("To test which was faster we ran the above function in "
        ++ "the :set +s enviroment. The results are printed below")
    print "test1 slow: exM' 5 99999999 19: (3.90 secs, 86,137,064 bytes)"
    print "test1 fast: exM 5 99999999 19: (0.00 secs, 65,328 bytes)" 
    print "test2 slow: exM' 7 999999999 17: (174.59 secs, 993,818,264 bytes)"
    print "test2 fast: exM 7 999999999 17: (0.00 secs, 65,328 bytes)" 
    print ("You can clearly see that the old methods increases very fast in time and size"
        ++ " while the new fast methods stay around the same running time and size")
        
-- 3 Time: 10 min
-- line 147 lecture 6

--4 Time: 1 hour

{-
    Using little Fermat: 
    return first fail of a identified prime that is not a prime

    with k repetitions
-}
firstFail :: Int -> [Integer] -> IO Integer
firstFail k (x:xs) = do
    isPrime <- primeTestsF k x
    next <- if isPrime then return x else firstFail k xs
    return $ next

-- list of first fail of found prime that is not a prime
fermatsCheck :: Int -> IO [Integer]
fermatsCheck maxK = do
    first <- firstFail maxK composites
    next <- if (maxK > 1) then fermatsCheck (maxK - 1) else return []
    return (next ++ [first])

assignment4 = do
    print "---- Assignment 4 - fermat"
    print "You see that as you increasing k first fail decreases really fast"
    fermat <- fermatsCheck 9
    print fermat

--5 Time: 30 min

{-
    Using the carmichael numbers you can see that the fermat
    checks fails a lot.

    Fermat's little theorem states that if p is a prime number,
    then for any integer b, the number b p − b is an integer 
    multiple of p. Carmichael numbers are composite numbers 
    which have this property. Carmichael numbers are also 
    called Fermat pseudoprimes or absolute Fermat pseudoprimes. 
    A Carmichael number will pass a Fermat primality test to 
    every base b relatively prime to the number, even though it 
    is not actually prime.
-}

-- Carmichael numbers
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
    k <- [2..], 
    prime (6*k+1), 
    prime (12*k+1), 
    prime (18*k+1) ]

{-
    Using little Fermat check the first carmichael numbers
    where the method falsely identifies a non-prime as prime
-}
carmichaelCheck :: Int -> IO [Integer]
carmichaelCheck maxK = do
    first <- firstFail maxK carmichael
    next <- if (maxK > 1) then carmichaelCheck (maxK - 1) else return []
    return (next ++ [first])
        
assignment5 = do
    print "---- Assignment 5 - carmichael"
    check <- carmichaelCheck 20
    print check
    print "You see that as you increasing k first fail does not decrease as fast as before"
    print "This is because carmichael numbers are special numbers: see code for details"

--6 Time: 2 Hour

-- Miller-Rabin primality check
{-
    Uses miller rabin method to check if a non-prime is identified as a prime.
    Returns the first non-prime were it went wrong

    Checks k times
    (x:xs) takes a list of non-primes to check from
-}
firstFailMiler :: Int -> [Integer] -> IO Integer
firstFailMiler k [] = error "No more non-primes to check from"
firstFailMiler k (x:xs) = do
    isPrime <- primeMR k x
    next <- if isPrime then return x else firstFailMiler k xs
    return $ next

{-
    uses the miller rabin method with different k's to check were the first
    non-prime is falsely identified as a prime

    makK : maximum repetitions
-}
millerTest :: Int -> IO [Integer]
millerTest maxK = do
    first <- firstFailMiler maxK carmichael
    next <- if (maxK > 1) then millerTest (maxK - 1) else return []
    return (next ++ [first])

mersPrimes :: [Integer]
mersPrimes = [2,3,5,7,13,17,19,31,61,89,107,127,521,607,1279,2203]

{- 
    Tries to find n mers primes
    (p:ps) -> List of primes to search from

    Check with miller robin is 2^p - 1 is an merse prime
    If it is, actually check if it is a merse prime with
    known data see => mersPrimes
-}
showMersPrimes :: Integer -> [Integer] -> IO ()
showMersPrimes n (p:ps) = do
    let mers = 2^p - 1
    isMersPrime <- primeMR 2 mers
    let message = if isMersPrime
        then print ("Found merse prime: " ++ show p ++ ": " ++ show mers)
        else return ()
    message
    let check = if isMersPrime 
        then if elem p mersPrimes
            then print ("Correct merse prime")
            else print ("Invalid merse prime!")
        else return ()
    check
    next <- if n > 0 || (head ps) > (maximum mersPrimes) then showMersPrimes 
                        (if isMersPrime then n - 1 else n) ps
                    else return ()
    return ()

assignment6 = do
    print "---- Assignment 6 - Miller Robin"
    print "Testing miller-rabin primality for k=1,2; 3 takes to long"
    miller <- millerTest 2
    print miller
    print ""
    print "Finding large mers primes"
    showMersPrimes 12 primes

main = do
    assignment1
    print ""
    print "---- Assignment 3 in lecture"
    print ""
    assignment4
    print ""
    assignment5
    print ""
    assignment6