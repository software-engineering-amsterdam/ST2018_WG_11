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

  -- exercise 5
primesTo10000 :: [Integer]
primesTo10000 = primesUntil 10000

generatePrimeFromList :: Integer
generatePrimeFromList = head ( filter isPrime [sum(take 101 (drop y primesTo10000)) | y <- [0..]])

testGeneratePrimeFromList :: Bool
testGeneratePrimeFromList = isPrime generatePrimeFromList
 
-- exercise 6
counterExamples :: [Integer]
counterExamples = [(product (take x primes) + 1) | x <- [1..] ,not (isPrime (product (take x primes) + 1))]

smallestCounterExample :: Integer
smallestCounterExample = head counterExamples

-- exercise 7
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

-- check for credit cards
isAmericanExpress :: Integer -> Bool
isAmericanExpress x = ((take 2 list) == [3,7] || (take 2 list) == [3,4]) && (luhn x) && ((length list) == 15)
      where list = (intToList x)

isMaster :: Integer -> Bool
isMaster x = ((length list) == 16) && (luhn x) && ((firstTwo >= 51 && firstTwo <= 55) || (firstFour >= 2221 && firstFour <= 2720))
      where list = (intToList x)
            firstTwo = (list !! 0) * 10 + (list !! 1)
            firstFour = (list !! 0) * 1000 + (list !! 1) * 100 + (list !! 2) * 10 + (list !! 3)

isVisa :: Integer -> Bool
isVisa x = ((length list) == 16) && ((head list) == 4) && (luhn x)
      where list = (intToList x)

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
  
-- exercise 8
data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- assume the first three tell the truth and the last two lie
-- checkCombination :: [Boys] -> Bool
-- check if all their statement imply
-- checkCombination bs = [bs | bs <- (perms boys)]


accuses :: Boy -> Boy -> Bool
accuses Matthew Carl = False
accuses Matthew Matthew = False
accuses Matthew _ = True
--Er is nog iets van een default case nodig ofzo
-- accuses Peter Matthew =  not (accuses Peter Jack)  --not both can be true
-- accuses Peter Jack = not (accuses Peter Matthew)     --not both can be true
-- Since the or is not exclusive i think both can be true
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Peter _ = False
-- we cant directly say who they accuse we can only say that who the other boy accuses is true or not
accuses Jack x = not (accuses Peter x) && not (accuses Matthew x)
-- Maybe create exclusive or function for this
accuses Arnold x = (accuses Peter x || accuses Matthew x) && not (accuses Peter x && accuses Matthew x)
accuses Carl x = not (accuses Arnold x)


accusers :: Boy -> [Boy]
accusers y = [x | x <- boys , accuses x y]
-- accusers Matthew = [Peter, Jack, Arnold]
-- accusers Peter = [Jack, Arnold]
-- accusers Jack = [Peter]
-- accusers Arnold = [Carl]
-- accusers Carl = []


guilty :: [Boy]
guilty = [x | x <- boys, length (accusers x) == 3]
honest :: [Boy]
honest = [y | x <- guilty, y <- (accusers x)]


-- gebruik permutations om verschillende combinaties van mensen die de waarheid spreken (120 opties: 5!)

main = do
  -- quickCheck myallTest
  -- quickCheck equalTest
  -- quickCheck testFuncs1
  -- quickCheck testFuncs2
  -- quickCheck testPrimePairs
  -- quickCheck(testPowerSet)
  -- print (perms [1,2,3])
  -- print ( factorial 3)
  -- quickCheck testGeneratePrimeFromList
  -- quickCheck testPerms
  -- print(generatePrimeFromList)
  -- print (perms [1,2,3])
  --print(primePairs 10000)

  -- testAllCards
  -- print (perms boys)
  print guilty
  print honest
