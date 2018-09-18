
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- 1 Red Curry random number generator test

redCurryTest :: IO [Int]
redCurryTest = do list <- probs 10000
                  return ([length [x | x <- list, x > 0 && x < 0.25],
                          length [x | x <- list, x >= 0.25 && x < 0.50],
                          length [x | x <- list, x >= 0.50 && x < 0.75],
                          length [x | x <- list, x >= 0.75 && x < 1]])

{-
      Since it is an open interval; (0..1). I would expect the
      first and last list to contain less items.

      Running the test a few times I can see that it differs every
      time with around ~50 in eacht quartile. The random generator
      seems to be working fine. I do not procces enough calculation 
      power to test my first hypothosis.
-}

-- 2 Recognizing triangles
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
    | x + y < z || y + z < x || x + z < y = NoTriangle
    | x == y && y == z = Equilateral
    | (x ^ 2) + (y ^ 2) == (z ^ 2) ||  (x ^ 2) + (z ^ 2) == (y ^ 2) ||  (z ^ 2) + (y ^ 2) == (z ^ 2)  = Rectangular
    | x == y || x == z || y == z = Isosceles
    | otherwise = Other

-- Three different NoTriangle cases
testNoTriangle1 :: (Positive Integer) -> (Positive Integer) -> (Positive Integer) -> Bool
testNoTriangle1 (Positive x) (Positive y) (Positive z) = (x + y < z) --> triangle x y z == NoTriangle

testNoTriangle2 :: (Positive Integer) -> (Positive Integer) -> (Positive Integer) -> Bool
testNoTriangle2 (Positive x) (Positive y) (Positive z) = (y + z < x) --> triangle x y z == NoTriangle

testNoTriangle3 :: (Positive Integer) -> (Positive Integer) -> (Positive Integer) -> Bool
testNoTriangle3 (Positive x) (Positive y) (Positive z) = (x + z < y) --> triangle x y z == NoTriangle

testEquilateral :: (Positive Integer) -> Bool
testEquilateral (Positive x) = triangle x x x == Equilateral

-- Three different Isosceles cases: (x x y) (x y x) (y x x)
testIsosceles1 :: (Positive Integer) -> (Positive Integer) -> Bool
testIsosceles1 (Positive x) (Positive y) = (x /= y && 2 * x >= y) --> triangle x x y == Isosceles

testIsosceles2 :: (Positive Integer) -> (Positive Integer) -> Bool
testIsosceles2 (Positive x) (Positive y) = (x /= y && 2 * x >= y) --> triangle x y x == Isosceles

testIsosceles3 :: (Positive Integer) -> (Positive Integer) -> Bool
testIsosceles3 (Positive x) (Positive y) = (x /= y && 2 * x >= y) --> triangle y x x == Isosceles

-- Pythagorean triangles
-- src: https://en.wikipedia.org/wiki/Integer_triangle#Angles_of_an_integer_triangle 
testRectangular1 :: (Positive Integer) -> (Positive Integer) -> Bool
testRectangular1 (Positive x) (Positive y) = (x > y) --> triangle (x^2 - y^2) (2 * x * y) (x^2 + y^2) == Rectangular

testRectangular2 :: (Positive Integer) -> (Positive Integer) -> Bool
testRectangular2 (Positive x) (Positive y) = (x > y) --> triangle (x^2 - y^2) (x^2 + y^2) (2 * x * y) == Rectangular

testRectangular3 :: (Positive Integer) -> (Positive Integer) -> Bool
testRectangular3 (Positive x) (Positive y) = (x > y) --> triangle (2 * x * y) (x^2 + y^2) (x^2 - y^2) == Rectangular
{- 
A triangle with the properties (a + 1) (b + 2) (a + b + 2) create 'Other' triangles,
as they cannot be placed in the other cases, but still is a triangle
(as long as a is the smallest number and b is the greater number)
-}
testOther :: (Positive Integer) -> (Positive Integer) -> Bool
testOther (Positive x) (Positive y) = let a = minimum[x,y]
                                          b = maximum[x,y]
                                      in (triangle (a + 1) (b + 2) (a + b + 2) == Other)

{-
The tests use most of the same functions as the function. This means that the domain we're testing in
is already expected to be (and restricted to!) the same as the domain of the formula. This makes verifying hard(er) in this
case. It does test if it holds for the cases within that specific domain (via quickCheck). The test
cases are reduced because of the restrictions in quickCheck. 

TODO: iets minder cryptisch uitleggen? :')
 -}


-- 3 Testing properties strength
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\x -> p x --> q x)
weaker   xs p q = stronger xs q p 

test1, test2, test3, test4 :: Int -> Bool
test1 = \x -> even x && x > 3
test2 = \x -> even x || x > 3
test3 = \x -> (even x && x > 3) || even x
test4 = even

testList = [(test1,"even x && x > 3"), (test2, "even x || x > 3"), (test3, "(even x && x > 3) || even x"), (test4,"even")]


sortTest [] = []
sortTest ((f,name):xs) = sortTest [(f1,n1) | (f1, n1) <- xs, stronger [-10..10] f1 f]
                            ++ [name] ++ 
                            sortTest [(f1,n1) | (f1, n1) <- xs, not (stronger [-10..10] f1 f)]

propertieStrength = sortTest testList


-- 4 Recognizing Permutations

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = ((length x) == 0) && ((length y) == 0)
                        where (x,y) = crossAway (xs, ys)

-- Cross away elements of each list against eachother. List should be empty in the end
crossAway :: Eq a => ([a], [a]) -> ([a], [a])
crossAway (xs, []) = (xs,[])
crossAway ([],ys) = ([],ys)
crossAway ((x:xs), ys) | (length ((delete x ys)) == (length ys)) = ([x],[])
                       | otherwise = crossAway (xs, (delete x ys))

-- generate a random array and test its permutations
-- becuase of the arbitrary type we have to specify the type of the input when using quickcheck
testPermutations :: Eq a => [a] -> Bool
testPermutations xs = (length xs < 8) --> all (\ys -> isPermutation ys xs) (permutations xs)
-- limit because the size of permutations can get very large

-- if ys is an element of the permutations of xs then this implies that
-- our function isPermutation must also be True
hoareTest :: Eq a => [a] -> [a] -> Bool
hoareTest xs ys = (elem ys (permutations xs)) --> isPermutation xs ys


{-
    TODO: TESTREPORT

    below example
-}

assignment4Report = do
    quickCheck (testPermutations :: [Int] -> Bool)
    print (hoareTest [1,2,3] [3,2,1])
    print (hoareTest [1,2,3] [3,3,3])
    print (hoareTest [1,2,3] [4,3,2,1])
    print (hoareTest [1,2,3,5] [4,3,2,1])
    print (hoareTest [1,2,3,3] [3,2,1])


-- 5 Recognizing and generating derangements

-- Create derangements
derangements :: [Integer] -> [[Integer]]
derangements xs = [x | x <- permutations xs, checkAllIndices x xs]

-- Check if a given (valid) permutation is a derangement
checkAllIndices :: [Integer] -> [Integer] -> Bool
checkAllIndices [] [] = True
checkAllIndices [] _ = False
checkAllIndices _ [] = False
checkAllIndices (x:xs) (y:ys) | x == y = False
                              | otherwise = checkAllIndices xs ys

-- Checks is orig is a derangement of derag
isDerangement :: [Integer] -> [Integer] -> Bool
isDerangement derag orig = elem derag (derangements orig)

-- Throw all perms out where one of it's indices matches the original
deran :: Integer -> [[Integer]]
deran n = [x | x <- permutations [0..n-1],checkAllIndices x [0..n-1]]


{-
    To number of derangements should be equal to the subfactorial of the number n.
    This counts when the list is [0..n].

    For computing purposes the pre condition check if the list is not too long
    The function to be test should be the deran function
    The post contition should test for the above mentioned property.

    For the isDerangement function I do not see an extra test function
    https://en.wikipedia.org/wiki/Derangement -> section "Counting derangements"
-}

-- https://rosettacode.org/wiki/Permutations/Derangements#Haskell
subfactorial :: Integer -> Integer
subfactorial 0 = 1
subfactorial 1 = 0
subfactorial n = (n - 1) * (subfactorial (n - 1) + subfactorial (n - 2))

testDerangements :: (Integer -> Bool) -> (Integer->[[Integer]]) -> ([[Integer]] -> Integer-> Bool) -> [Integer] -> Bool
testDerangements _ _ _ [] = True
testDerangements precondition f postcondition (n:ns) = precondition n --> (if (postcondition (f n) n) then (testDerangements precondition f postcondition ns) else False)

checkDerangements :: Integer -> Bool
checkDerangements len = testDerangements (\x -> x < 8) deran (\ xs n -> (toInteger (length xs)) == subfactorial n) [0..len]

assignment5Report = do
    quickCheck checkDerangements


-- 6 Implementing and testing ROT13 encoding

{- 
    rot13 is an algorihm that takes a string and rotes it with a difference
    of 13 characters.
-}

rot13 :: String -> String
rot13 [] = []
rot13 (x:xs) | x >= 'a' && x < 'n' ||
               x >= 'A' && x < 'N' = chr (ord x + 13) : rot13 xs
             | x >= 'n' && x <= 'z' ||
               x >= 'N' && x <= 'Z' = chr (ord x - 13) : rot13 xs
             | otherwise = x : rot13 xs
{-
    manual tests for rot13.
-}
smallAlphabet = "abcdefghijklmnopqrstuvwxyz1234567890"
largeAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
rotatedSmall = "nopqrstuvwxyzabcdefghijklm1234567890"
rotatedLarge = "NOPQRSTUVWXYZABCDEFGHIJKLM1234567890"

-- The length of an encoded string must be the same as the initial one
rot13LengthTest :: String -> Bool
rot13LengthTest xs = length xs == length (rot13 xs)

{-
    When the string is bigger then the empty string and contains a alphabetic
    character it should not be the same as the initial string and when you
    rotate the string twice the same initial result should come out.
-}
rot13Test :: String -> Bool
rot13Test xs = ((length cs) > 0) && isAlpha (head cs) -->  (rot13 cs) /= cs && (rot13 (rot13 cs)) == cs
                where cs = show xs

                
assignment6Report = do
    print (rot13 smallAlphabet)
    print (rot13 largeAlphabet)
    print (rot13 smallAlphabet == rotatedSmall)
    print (rot13 largeAlphabet == rotatedLarge)
    print (rot13 (rot13 smallAlphabet) == smallAlphabet)
    print (rot13( rot13 largeAlphabet) == largeAlphabet)
    quickCheck rot13LengthTest
    quickCheck rot13Test


-- 7 Implementing and testing IBAN validation

--move 4 first characters to end of string
moveF4ToBack :: [Char] -> [Char]
moveF4ToBack xs = (drop 4 noSpaces) ++ (take 4 noSpaces)
                where noSpaces = (filter (/=' ') xs)

--replace all letters with numbers a=10,b=11,z=35
lettersToIntegers :: [Char] -> [Char]
lettersToIntegers [] = []
lettersToIntegers (c:cs) = if isUpper c then 
                                show((ord c - 65) + 10) ++ (lettersToIntegers cs)
                            else if (isLower c) then 
                                show((ord c - 97) + 10) ++ (lettersToIntegers cs)
                            else ([c] ++ (lettersToIntegers cs))

iban :: String -> Bool
iban cs = ((read (lettersToIntegers (moveF4ToBack cs)) :: Integer) `mod` 97) == 1

-- Test results

{-
    Increment Number With One
    Makes iban invalid: Takes a string with at least one Number and 
    increments the first number with one.
-}
inwo :: String -> String
inwo (x:xs) | isNumber x = chr ((ord x - ord '0' + 1) `mod` 10 + ord '0') : xs
            | otherwise = x : inwo xs


-- One valid iban number for every country
validIbanNumbers :: [String]
validIbanNumbers = 
    [
        "GB82 WEST 1234 5698 7654 32", "FR76 3000 6000 0112 3456 7890 189", "AD10994158463H4ZS9BKSQ65","AE591822963255324364383","AL6295496225TL97CH7QAESHOCP5","AT216335991632561698","AZ55ZCYNFJ22WDZKM8OKC0JSIK4F","BA262178601039419450","BE14370109142531",
        "BG70ORDZ776985APOODSHZ","BH57AUTMC40QH4HCCQSNJN","BR2357520282083962316112424LG","BY91TLD53539BJ84L5V8RHEN8KMW","CH5230063XTRNGTE648M0","CR28187161928892693512","CY8496981181O2EWKSBPEBIF848R","CZ0761488919151612079444","DE98756235360674485858","DK1062569766766675",
        "DO917DGD50813360262443123799","EE156536313403026507","ES8055642092242062212194","FI2784072492308811","FO0213086537460370","FR074032936702MDZUNJMRV5E52","GB33SCEQ98400924784542","GE37JX0298197643873741","GI35KQOX0XA53DD1APIRFMZ","GL9492929394922044",
        "GR136224543F7I5HKL5FXDDCRAO","GT863D2WSIFS8JZ0VRRW7B9I7VEZ","HR2613554422026293540","HU78309067413183010510072390","IE57JIJA31632031092654","IL510578567620066644649","IQ45BACL961447512757450","IS438195278054672813073602","IT82A7879638917I2RCF3V0ZM2W",
        "JO95MTBQ8611F2H8PD2F9GO64DWKES","KW80JUSV11FMQI0B1GA43HV8NKCK4V","KZ68567ARY9RVP74UP5A","LB0614524DG6R6HJIE27Q4KOVDWQ","LC32CWCLH935JZJI5C78SRP5JW879HDW","LI5619572JP6Z5TGVR0MS","LT478164414733309513","LU23519COA3QCQ6UAK9S","LV77QUUYHM2AM9ZGQ5LYL",
        "MC426509657012QTVC2404LWH12","MD980C127HWEGJCLKJ8X2FYD","ME92753107624316441002","MK4574625LXGQUX9779","MR6762766970495095294641778","MT88GQIV47519WOQFFFXWH5OQL9MPCA","MU65USLU1656086023066824176ZPF","NL05BEGJ9694051827","NO2431091366443",
        "PK75RASEREEXSOI428ENAYSJ","PL67526955058889200858865767","PS68BHJUZQ72FWMTC48QUBU8JT8RQ","PT77693451383347314578360","QA68AZSUORX8JO6VAXTTUPM1VSYVA","RO14WEVXH2YNBQQ9WCHRXVUQ","RS14067377404888804490","SA8612YYCCGFWA8LA2AW50XR",
        "SC86NENG08315757602506778678VVF","SE3744811917217558328936","SI33166388647584194","SK7992529811526047730440","SM42V1626343439TMICRFB4R0EZ","ST07896783411053404784123","SV40ORDQ27862726929082958547","TL277407479946101816469",
        "TN5713115293410957659500","TR1728531AXG84ELL0GPMIUFVZ","UA82354230YYJY5SMFQ6BHCUXW1PF","VG03SONO9086118580102521","XK246070182276188083","YY03RZIQ015939616CQJYSB9DTOBAT13CY","ZZ84GEQC772206256GCUG5ANE7V3K8VK2J2"
    ]

-- checks hardcoded ibans and checks whether all are correct
ibanValidCheck = all (\x -> x == True) [iban x | x <- validIbanNumbers]

-- Takes hardcoded ibans, increments the first occurance of a number by 1 
-- (making it invalid) and checks wether iban returns False as it should
ibanInValidCheck = all (\x -> x == False) [iban (inwo x) | x <- validIbanNumbers]

{-
    Can you automate the test process?
    Yes you can generate correct and incorrect iban numbers:
    https://en.wikipedia.org/wiki/International_Bank_Account_Number 

    Incorrect testing: 
    You can increment one or more numbers with one, since it is a mod 97 
    it will always fail.
-}

assignment7Report = do
    print ibanValidCheck
    print ibanInValidCheck

main = do
    assignment1 <- redCurryTest
    print "Assignment 1 red curry"
    print assignment1
    
    print "Assignment 2 Triangles"
    print "Checking Equilateral Triangles"
    quickCheck testEquilateral

    print "Checking Three Isosceles Triangle Cases"
    print "case1: x == y"
    quickCheck testIsosceles1
    print "case2: x == z"
    quickCheck testIsosceles2
    print "case3: y == z"
    quickCheck testIsosceles3

    print "Checking Three Rectangular Triangle Cases"
    print "case1: (x ^ 2) + (y ^ 2) == (z ^ 2)"
    quickCheck testRectangular1
    print "case2: (x ^ 2) + (z ^ 2) == (y ^ 2)"
    quickCheck testRectangular2
    print "case3: (z ^ 2) + (y ^ 2) == (z ^ 2)"
    quickCheck testRectangular3

    print "Checking Other Triangles"
    quickCheck testOther

    print "Checking Three NoTriangle Cases"
    print "case1: x + y < z"
    quickCheck testNoTriangle1
    print "case1: y + z < x"
    quickCheck testNoTriangle2
    print "case1: x + z < y"
    quickCheck testNoTriangle3

    print "Asssignment 3 properties strength"
    print propertieStrength
    print "Asssignment 4 Recognizing Permutations"
    assignment4Report
    print "Asssignment 5 Recognizing and generating derangements"
    assignment5Report
    print "Asssignment 6 Implementing and testing ROT13 encoding"
    assignment6Report
    print "Asssignment 7 Implementing and testing IBAN validation"
    assignment7Report
