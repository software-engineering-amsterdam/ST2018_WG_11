module Lab6 where 
import Data.List
import Lecture6
import Test.QuickCheck

-- 1 Time: 2 hours

{-
    source: https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/fast-modular-exponentiation 
    the operation of modular exponentiation calculates the 
    remainder when an integer b (the base) raised to the eth 
    power (the exponent), be, is divided by a positive integer
    m (the modulus)

    b^e mod m

    A^2 mod C = (A * A) mod C = ((A mod C) * (A mod C)) mod C
-}

{-    
    b > 0 is the base 
    e > 0 is the exponent
    m > 0 is the modulo

    b^e `mod` m = (b^(biggestPowerOf2) * b^(rest)) `mod` m
        = ((b^(biggestPowerOf2) `mod` m) * b^(rest)) `mod` m
-}
myExM :: Integer -> Integer -> Integer -> Integer
myExM b e m = (squareMod b powerOfTwo m * rest) `mod` m
                where 
                    powerOfTwo = until (\x -> x * 2 > e) (\x -> x * 2) 1
                    rest = if (e - powerOfTwo) > 8 then myExM b (e - powerOfTwo) m
                            else (b ^ (e - powerOfTwo))

{-
    Fast calculation of b^e mod m
    !! ==> e must be in the power of 2

    b^2 `mod` m  = (b^1 * b^1) `mod` m = (b^1 `mod` m) * (b^1 `mod` m) `mod` m
    b^4 `mod` m  = (b^2 * b^2) `mod` m = (b^2 `mod` m) * (b^2 `mod` m) `mod` m
    etc
    b^n `mod` m  = (b^(n/2) * b^(n/2)) `mod` m = (b^(n/2) `mod` m) * (b^(n/2) `mod` m) `mod` m

-}
squareMod :: Integer -> Integer -> Integer -> Integer
squareMod b 1 m = b `mod` m
squareMod b e m = (value * value) `mod` m
                    where
                        value = squareMod b (e `div` 2) m

{-
    quickCheck function to test correctness
-}
mytest :: Integer -> Integer -> Integer -> Bool
mytest b e m = b < 1 || e < 1 || m < 1 || (exM b (e*999) m) == (myExM b (e*999) m)

{-
    test version to compare runtime
-}
test1 = exM 9 9999999 13
test2 = myExM 9 9999999 13


assignment2 = do
    print "Testing runtime of different functions and correctness"
    quickCheck (withMaxSuccess 10000 mytest)
    print ("To test which was faster I ran the above function in "
        ++ "the :set +s enviroment the results are printed below")
    print "exM 9 9999999 13: (0.28 secs, 11,605,312 bytes)"
    print "myExM 9 9999999 13: (0.00 secs, 168,288 bytes)" 