module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd


-- 1. Questions 'The Haskell Road' Chapter 4. Time: +/- 2 hours

{-
    1. Example 4.6: There is no set corresponding to the property F (x)... too arbitrary! (page 121)
    2. The halting problem is not entirely clear: how exactly can 'undefined' be used? 
    3. Difficulty of using lists as sets: they have different identity conditions.
       Sets are unordered, lists are ordered, but we can use lists to represent finite
       (or countably infinite) sets by representing sets as lists with duplicates removed, 
       and by disregarding the order. What are some limitations still? Not clear! (page 149)
       The only one given: 
       The representation of sets as lists without duplicates has the drawback that two finite lists 
       containing the same elements, but in a different order, e.g., [1,2,3] and [3,2,1], are unequal 
       as lists, but equal as sets. The Haskell equality operator == gives the wrong results when we 
       are interested in set equality.
       >> we solve this by defining a special data type for sets, with a matching definition of equality


       Keep in mind:
       - The Haskell operation for list difference is predefined as \\ in List.hs
-}

-- 2. Random Data Generator. Time: 30-35 mins
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
noDuplicates []     = True
noDuplicates (x:xs) = if x `elem` xs then False else noDuplicates xs


isValid :: (Ord a) => (Set a) -> Bool
isValid (Set a) = (isSorted a == True) && (noDuplicates a == True)

assignment2 = do
    print "Assignment 2: Random Test Set Int"
    -- generate arbitrary :: IO (Set (Int))
    quickCheck (isValid :: Set Int -> Bool)


-- setToList (Set a) = a

-- 3. Operations for set intersection, set union and set difference. Time: 50 +... min

xor :: Bool -> Bool -> Bool
xor x y | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False

diffSet :: (Ord a) => Set a -> Set a -> Set a
diffSet (Set a) (Set b) = Set (sort((a \\ b) ++ (b \\ a)))

testDiff :: (Ord a) => Set a -> Set a -> Bool
testDiff (Set a) (Set b) = all(==True) ([if xor (x `elem` a) (x `elem` b) then x `elem` diffList else not(x `elem` diffList) | x <- fullList])
          where diffList = (\(Set x) -> x) (diffSet (Set a) (Set b))
                fullList = diffList ++ a ++ b

-- what elements do the sets share? Is it still ordered?
interSet :: (Ord a) => Set a -> Set a -> Set a
interSet (Set xs) set2 = Set ([x | x <- xs, inSet x set2])

testInter :: (Ord a) => Set a -> Set a -> Bool
testInter (Set a) (Set b) = all(==True) ([if x `elem` a && x `elem` b then x `elem` interList else not(x `elem` interList) | x <- fullList])
          where interList = (\(Set x) -> x) (interSet (Set a) (Set b))
                fullList = interList ++ a ++ b

-- set union already exists..? Just test it!
-- unionTest :: (Ord a) => Set a -> Set a -> Bool

assignment3 = do
    print "assignment3"
    print "Test set difference"
    quickCheck (testDiff :: Set Int -> Set Int -> Bool)

    print "Test set intersection"
    quickCheck (testInter :: Set Int -> Set Int -> Bool)


-- 4. Questions 'The Haskell Road' Chapter 5. Time: ...

-- 5. Function for symmetric closure of a relation. Time: 15 mins
type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [(x, y)]    = [(x, y)] ++ [(y, x)]
symClos ((x, y):xs) = nub((x, y):(y, x):(symClos xs))

-- symClos' :: Ord a => Rel a -> Rel a
-- symClos' [x] = [x]
-- symClos' ((x, y):xs) = if (y, x) `elem` (x, y):xs
--                       then (x, y):(y, x):(symClos' xs) -- skip over existing syms >> DIT GAAT DUS NOG NIET GOED
--                       else (x, y):(y, x):(symClos' xs) -- add sym to list, continue


-- 6. Function that gives the transitive closure of a relation. Time: ...
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- trClos :: Ord a => Rel a -> Rel a 

-- 7. Test the functions symClos and trClos. Time: ...

-- 8. Is there a difference between the symmetric closure of the transitive closure of a relation R and the transitive closure of the symmetric closure of R?

main = do
    assignment2
    assignment3