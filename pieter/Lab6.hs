module Lab6 where

import Data.List
import Data.Char
import System.Random
import Lecture6
import Test.QuickCheck

import Control.Monad
import System.CPUTime

import Numeric (showIntAtBase)


infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- Exercise 1
-- 180 minutes
-- Sources:
-- https://en.wikipedia.org/wiki/Modular_exponentiation
-- https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/fast-modular-exponentiation

-- First solution wikipedia gives but this isnt the one we want
-- Wikipedia says this is the memory efficient method
exM' :: Integer -> Integer -> Integer -> Integer
exM' b e m = helpExM' b e m 1 0

helpExM' :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
helpExM' b e m c e_prime | e_prime < (e-1) = helpExM' b e m nextC (e_prime + 1)
                     | otherwise = nextC
    where nextC = (c * b) `mod` m

-- Updated in the Lecture6.hs code
-- -- Better solution from khanacedemy
-- exM'' :: Integer -> Integer -> Integer -> Integer
-- exM'' base exp m = (helpExM'' base 1 m (helper exp)) `mod` m

-- helpExM'' :: Integer -> Integer -> Integer -> [Bool] -> Integer
-- helpExM'' _ _ _ [] = 1
-- helpExM'' base offset m (c:cs) | c = (simplifyExp base offset m) * (helpExM'' base (2*offset) m cs)
--                          | otherwise = helpExM'' base (2*offset) m cs

-- -- simplify a large power to multiple smaller ones
-- simplifyExp :: Integer -> Integer -> Integer -> Integer
-- simplifyExp base 1 m = base `mod` m
-- simplifyExp base exp m = (new^2) `mod` m
--               where new = simplifyExp base (exp `div` 2) m

-- -- Translate an integer to a list of booleans (binary)
-- -- https://stackoverflow.com/a/9166342
-- helper :: Integer -> [Bool]
-- helper 0 = []
-- helper n | n `mod` 2 == 1 = True : helper (n `div` 2)
--          | n `mod` 2 == 0 = False : helper (n `div` 2)

-- Simple function to test equivalence with original
testExM :: Integer -> Integer -> Integer -> Bool
testExM b e m = (b>0 && e>0 && m>0) --> (exM'' b e m) == (exM b e m)
 
-- Exercise 2
-- 60 minutes

assignment1And2 = do
  start <-getCPUTime
  (exM'' 9 9999999 13) `seq` return ()
  end1 <- getCPUTime
  (exM 9 9999999 13) `seq` return ()
  end2 <- getCPUTime

  let diff1 = (fromIntegral (end1 - start)) / (10^12)
  let diff2 = (fromIntegral (end2 - end1)) / (10^12)
  let multiplier = (fromIntegral (end2 - end1)) / (fromIntegral (end1 - start))
  putStrLn "Exercise 1 and 2"
  putStrLn "Compare new function with old function on time hard example."
  putStrLn ("   New function time " ++ (show diff1))
  putStrLn ("   Old function time " ++ (show diff2))
  putStrLn ("New function is "++(show multiplier)++" times faster.")
  putStrLn ("\nMemory use is about 50 times better. Can be observed with :set +s")

  putStrLn "\nCheck equivalence between new function and original."
  quickCheck testExM


-- Exercise 3
-- 10 minutes

-- Added this function to Lecture6.hs
-- composites :: [Integer]
-- composites = [x | x<-[1..],not (prime x)]

assignment3 = do
  putStrLn "\nExercise 3"
  putStrLn "Print the first 100 composites."
  print $ take 100 composites

-- Exercise 4
-- Upto now 120 minutes

prime_tests_F :: Int -> [Integer] -> IO Integer
prime_tests_F k (x:xs) = do
  b <- primeTestsF k x
  if b then
    return x
  else
    prime_tests_F k xs

assignment4 = do
  prime_tests_F 1 composites
  prime_tests_F 2 composites
  prime_tests_F 3 composites
  prime_tests_F 4 composites
  prime_tests_F 10 composites

-- Exercise 5
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
              k <- [2..], 
              prime (6*k+1), 
              prime (12*k+1), 
              prime (18*k+1) ]

assignment5 = do
  prime_tests_F 1 carmichael
  prime_tests_F 2 carmichael
  prime_tests_F 3 carmichael
  prime_tests_F 4 carmichael
  prime_tests_F 10 carmichael          

-- Exercise 6.1

prime_tests_M :: Int -> [Integer] -> IO Integer
-- prime_tests_M k [] = do return 0
prime_tests_M k (x:xs) = do
  b <- primeMR k x
  if b then
    return x
  else
    prime_tests_M k xs

assignment6_1 = do
  prime_tests_M 1 carmichael
  prime_tests_M 2 carmichael
  prime_tests_M 3 carmichael
  -- prime_tests_M 4 carmichael

-- Exercise 6.2
-- (2^n)-1= prime

-- assignment6_2 = do
--   p <- prime_tests_M 3 carmichael
--   b <- prime (2^p-1)

test :: Int -> [Integer] -> IO Integer
test k [] = do return 2
test k (x:xs) = do
  p <- prime_tests_M k [x]
  let b = prime (2^p-1)
  if b then return p else test k xs




main = do
  assignment1And2
  assignment3