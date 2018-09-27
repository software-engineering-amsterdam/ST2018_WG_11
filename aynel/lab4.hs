module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd


-- 1. Questions 'The Haskell Road' Chapter 4. Time: ..

-- 2. Random Data Generator. Time: 25 mins
instance (Arbitrary a, Ord a, Eq a) => Arbitrary (Set a) where
    arbitrary = do
        list <- arbitrary
        return (Set (nub(sort(list))))

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = all id . map (\(x,y) -> x <= y) . zip xs $ tail xs

isSorted' :: (Ord a) => [a] -> Bool
isSorted' []       = True
isSorted' [x]      = True
isSorted' (x:y:xs) = x <= y && isSorted' (y:xs)

noDuplicates :: (Eq a) => [a] -> Bool
noDuplicates [] = True 
noDuplicates (x:xs) = if x `elem` xs then False else noDuplicates xs


isValid :: (Ord a) => (Set a) -> Bool
isValid (Set a) = (isSorted a == True) && (noDuplicates a == True)

assignment2 = do
    print "Assignment 2: Random Test Set Int"
    -- generate arbitrary :: IO (Set (Int))

    quickCheck (isValid :: Set Int -> Bool)



-- 3. Operations for set intersection, set union and set difference

-- 4. Questions 'The Haskell Road' Chapter 5. Time: ..

-- 5. Function for symmetric closure of a relation, where the relation is represented as an ordered list of pairs