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

myall :: (a ->Bool) -> [a] ->Bool
myall p [] = True
myall p (x:xs) = p x && myall p xs

--elem check if element is in list
list2p ::Eq a => [a] -> a ->Bool
list2p = flip elem

myallTest :: [Int] -> [Int] ->Bool
myallTest = \ ys xs ->let p = list2p ys in  all p xs == myall p xs

isEqual :: Int -> Int -> Bool
isEqual x y = (x == y)

equalTest :: Int -> Int -> Bool
equalTest = \ y x -> (isEqual x y) == (x == y)

--exercise 1
func1_1 :: Int -> Int
func1_1 n = (n * (n + 1) * (2 * n + 1)) `div` 6

func1_2 :: Int -> Int
func1_2 0 = 0
func1_2 x = (x * x) + (func1_2 (x-1)) 

testFuncs1 :: Int -> Bool
testFuncs1 = \ y -> if y >= 0 then (func1_1 y) == (func1_2 y) else (func1_1 (-y)) == (func1_2 (-y))

func2_1 :: Int -> Int
func2_1 n = ((n * (n + 1)) `div` 2) * ((n * (n + 1)) `div` 2)

func2_2 :: Int -> Int
func2_2 0 = 0
func2_2 x = (x * x * x) + (func2_2 (x-1))

testFuncs2 :: Int -> Bool
-- testFuncs2 = \ y -> if y >= 0 then (func2_1 y) == (func2_2 y) else (func2_1 (-y)) == (func2_2 (-y))
testFuncs2 = \ y -> y >=0 -->  (func2_1 y) == (func2_2 y)

-- exercise 2
-- not hard to test because this is equal to 2^(number of elements in original list)

powerset :: [a] -> [[a]]
powerset xs = subsequences xs

testPowerSet :: [Integer] -> Bool
testPowerSet = \xs -> ((length xs) <= 25) --> (length (powerset xs)) == (2^(length xs))

-- excercise 3
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

testPerms :: [Int] -> Bool
testPerms xs = ((length xs) <= 10) --> ((length (perms xs)) == (factorial (length xs)))

-- exercise 4
isPrime :: Integer -> Bool
isPrime x = (length [y | y <- [2..(x-1)],(x `mod` y) == 0]) == 0

primesUntil :: Integer -> [Integer]
primesUntil x = [y | y <- [2..x],isPrime y]

primePairs :: Integer -> [(Integer, Integer)]
primePairs x = [(y,reversal y) | y <- (primesUntil x), isPrime (reversal y)]

-- how would you test this function?
testPrimePairs :: Integer -> Bool
testPrimePairs = \ y -> length ([(a,b) | (a,b) <- (primePairs y), 
                    (a == (reversal b)) && (isPrime a) && (isPrime b)]) == (length (primePairs y))
--it does check if the pairs are correct but not if these are all the pairs that exist


primesTo10000 :: [Integer]
primesTo10000 = primesUntil 10000
-- exercise 5
testSome :: Integer
testSome = head ( filter isPrime [sum(take 101 (drop y primesTo10000)) | y <- [0..]])
-- i dont know how to test this
 
-- exercise 6



main = do
  -- quickCheck myallTest
  -- quickCheck equalTest
  -- quickCheck testFuncs1
  -- quickCheck testFuncs2
  -- quickCheck testPrimePairs
  -- quickCheck(testPowerSet)
  print (perms [1,2,3])
  print ( factorial 3)
  quickCheck testPerms
  -- print(testSome)
  -- print (perms [1,2,3])
  --print(primePairs 10000)
  

