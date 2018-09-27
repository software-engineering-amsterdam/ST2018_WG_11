module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd


-- 1. Questions 'The Haskell Road' Chapter 4. Time: ..

-- 2. Random Data Generator. Time: 30 mins
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



-- 3. Operations for set intersection, set union and set difference. Time: ...

-- 4. Questions 'The Haskell Road' Chapter 5. Time: ...

-- 5. Function for symmetric closure of a relation, where the relation is represented as an ordered list of pairs. Time: ...
type Rel a = [(a,a)]

-- symInList :: Ord a => a -> Rel a -> Bool
-- symInList n xs = n `elem` xs

-- symClos' :: Ord a => Rel a -> Rel a
-- symClos' [x] = [x]
-- symClos' ((x, y):xs) = if (y, x) `elem` (x, y):xs
--                       then (x, y):(y, x):(symClos' xs) -- skip over existing syms >> DIT GAAT DUS NOG NIET GOED
--                       else (x, y):(y, x):(symClos' xs) -- add sym to list, continue

symClos :: Ord a => Rel a -> Rel a
symClos [(x, y)] = [(x, y)] ++ [(y, x)]
symClos ((x, y):xs) = nub((x, y):(y, x):(symClos xs))


-- 6. Use the datatype for relations and define a function that gives the transitive closure of a relation. Time: ...

-- 7. Test the functions symClos and trClos. Time: ...

-- 8. Is there a difference between the symmetric closure of the transitive closure of a relation R and the transitive closure of the symmetric closure of R?

main = do
    assignment2