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
mytest :: Integer -> Integer -> Integer -> Bool
mytest b e m = e < 0 || m == 0 || (b^(e) `mod` m == (exM b (e) m))

{-
    Old method from lecture to compare running time
-}
exM' :: Integer -> Integer -> Integer -> Integer
exM' = expM

test1_slow = exM' 9 9999999 13
test1_fast = exM 9 9999999 13
test2_slow = exM' 9 99999999 13
test2_fast = exM 9 99999999 13

assignment1 = do
    print "---- Assignment 1 & 2"
    print "Testing runtime of different functions and correctness"
    quickCheck (withMaxSuccess 10000 mytest)
    print ("To test which was faster I ran the above function in "
        ++ "the :set +s enviroment the results are printed below")
    print "test1 slow: exM' 9 9999999 13: (0.28 secs, 11,605,312 bytes)"
    print "test1 fast: exM 9 9999999 13: (0.00 secs, 168,288 bytes)" 
    print "test2 slow: exM' 9 99999999 13: (4.38 secs, 117,611,984 bytes)"
    print "test2 fast: exM 9 99999999 13: (0.00 secs, 219,048 bytes)" 
    print ("You can clearly see that the old methods increases very fast in time and size"
        ++ " while the new fast methods stay around the same running time")

-- 3 
-- line 147 lecture 6

--4 time: 30 min

-- returns first fail of prime that is not a prime
-- with k repetitions
firstFail :: Int -> [Integer] -> IO Integer
firstFail k (x:xs) = do
    isPrime <- primeTestsF k x
    next <- if isPrime then return x else firstFail k xs
    return $ next

-- Reversed list of first fail of found prime that is not a prime
fermatsCheck :: Int -> IO [Integer]
fermatsCheck maxK = do
    first <- firstFail maxK composites
    next <- if (maxK > 1) then fermatsCheck (maxK - 1) else return []
    return (first : next)

assignment4 = do
    print "---- Assignment 4 - fermat"
    print "You see that as you increasing k first fail decreases really fast"
    check <- fermatsCheck 9
    print $ reverse $ check

--5

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

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
    k <- [2..], 
    prime (6*k+1), 
    prime (12*k+1), 
    prime (18*k+1) ]

carmichaelCheck :: Int -> IO [Integer]
carmichaelCheck maxK = do
    first <- firstFail maxK carmichael
    next <- if (maxK > 1) then carmichaelCheck (maxK - 1) else return []
    return (first : next)
        
assignment5 = do
    print "---- Assignment 5 - carmichael"
    check <- carmichaelCheck 20
    print $ reverse $ check
    print "You see that as you increasing k first fail does not decrease as fast as before"
    print "This is because carmichael numbers are special numbers: see code for details"

--6
{-
    Use the list from the previous exercise to test the 
    Miller-Rabin primality check. What do you find?

    find: n-1 = 2^k * m

    2) a^m `mod` n

    3) p = (1 , k - 1)
-}

-- myFunc n m p k = if b == 1 then False else 
--                 if b == -1 then True else 
--                     if p == k - 1 then False else myFunc (p + 1) b m k
--                 where b = n^2 `mod` m

-- miller :: Integer -> IO Bool
-- miller n = do
--     a <- randomRIO (1, n-1) :: IO Integer
--     let k = gcd (n - 1) a
--     let (m, x) = quotRem (n - 1) (a ^ k)
--     print (show n ++ " " ++ show a ++ " " ++ show m ++ " " ++ show k)
--     let bo = a ^ m `mod` n
--     let prime = if bo == 1 then True else
--                     if bo == -1 then True else myFunc bo n 0 k
--     return $ prime

-- millerTest :: [Integer] -> IO Integer
-- millerTest (x:xs) = do
--     result <- miller x
--     final <- if result then millerTest xs else return x
--     return $ final

firstFailMiler :: Int -> [Integer] -> IO Integer
firstFailMiler k (x:xs) = do
    isPrime <- primeMR k x
    next <- if isPrime then return x else firstFailMiler k xs
    return $ next

millerTest :: Int -> IO [Integer]
millerTest maxK = do
    first <- firstFailMiler maxK carmichael
    next <- if (maxK > 1) then millerTest (maxK - 1) else return []
    return (first : next)

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
    millerTest 2
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