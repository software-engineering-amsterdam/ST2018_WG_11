
module Lab4
where 
import Data.List
import System.Random
import Test.QuickCheck
import Lecture4
import SetOrd


-- 2
{-
    Implement a random data generator for the datatype Set Int,
    where Set is as defined in SetOrd.hs. First do this from 
    scratch, next give a version that uses QuickCheck to random 
    test this datatype.
-}

-- Removes duplicates and sorts from lesser to greater
quickSort :: (Eq a, Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort lesser) ++ [x] ++ (quickSort greater) where
    lesser = filter (\y -> y < x) xs
    greater = filter (\y -> y > x) xs

-- From: http://geekyplatypus.com/y-u-have-no-code-samples-quickcheck/
instance (Arbitrary a, Ord a, Eq a) => Arbitrary (Set a) where
    arbitrary = do
                list <- arbitrary
                return $ Set (quickSort list)

randomIntSet :: IO (Set Int)
randomIntSet = generate arbitrary :: IO (Set Int)

containsDupSet :: (Ord a) => Set a -> Bool
containsDupSet (Set []) = True
containsDupSet (Set (x:xs)) = (not (inSet x (Set xs))) && 
    (containsDupSet (Set xs))

isOrdSet :: (Ord a) => Set a -> Bool
isOrdSet (Set []) = True
isOrdSet (Set [_]) = True
isOrdSet (Set [x,y]) = x < y
isOrdSet (Set (x:y:ys)) = x < y && isOrdSet (Set (y:ys))

isValidSet :: (Ord a) => Set a -> Bool
isValidSet set = (containsDupSet set) && (isOrdSet set)

assignment2 = do
    print ("Assignment 2")
    quickCheck (isValidSet :: Set Int -> Bool)

main = do
    assignment2