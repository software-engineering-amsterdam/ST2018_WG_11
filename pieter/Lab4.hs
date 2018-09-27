module Lab4 where

import Data.List
import Data.Char
import Data.Tuple
import Test.QuickCheck
import SetOrd
import System.Random

-- Exercise 1 Read Chapter 4 of The Haskell Road
-- 90 minutes
-- Pretty much everything was clear but some exercises and 
-- examples were vague like exercise 4.33 and 4.34

-- Exercise 2
-- 60 minutes
instance (Arbitrary a,Ord a) => Arbitrary (Set a) where
    arbitrary = do
        xs <- arbitrary
        return (Set (sort (nub xs)))

genList :: IO (Set Int)
genList = do
    length <- (randomRIO (1,20))
    xs <- (randomList length)
    return (Set xs)

-- credit https://stackoverflow.com/a/30741139
randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (-100,100)
  rs <- randomList (n-1)
  return (r:rs)   

-- simple test functions to see if quickcheck works
doubleList :: Set Int -> Set Int
doubleList (Set xs) = Set (map (*2) xs)

testDouble :: Set Int -> Bool
testDouble (Set xs) = (doubleList (Set xs)) == Set (map (*2) xs)

-- This is already implemented in the provided SetOrd library
-- unionSet :: (Ord a) => Set a -> Set a -> Set a 
-- unionSet (Set [])     set2  =  set2
-- unionSet (Set (x:xs)) set2  = 
--    insertSet x (unionSet (Set xs) set2)

-- Exercise 3 60 minutes

intersectionSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set []) _ = Set []
intersectionSet _ (Set []) = Set []
intersectionSet (Set xs) (Set ys) = Set [z | z <- xs, z `elem` ys]

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set []) ys = ys
differenceSet xs (Set []) = xs
differenceSet (Set xs) (Set ys) = Set ([p | p <- xs, not( p `elem` ys)] ++ [q | q <- ys, not( q `elem` xs)])

ts1 = Set [1,2,3,4,5]
ts2 = Set [4,5,6,7]

-- intersectionTest :: (Ord a) => Set a -> Set a -> Bool
-- intersectionTest (Set []) ys = (intersectionSet (Set []) ys) == Set []
-- intersectionTest xs (Set []) = (intersectionSet xs (Set [])) == Set []
-- intersectionTest (Set xs) (Set ys) = all (==True) ([(x `elem` intersection) && (not (x `elem` difY)) | x <- xs] ++
--                                      [(y `elem` intersection) && (not (y `elem` difX)) | y <- ys])
--                                         where (Set intersection) = intersectionSet (Set xs) (Set ys)
--                                               difX = intersection \\ ys
--                                               difY = intersection \\ xs

-- Test intersection
-- Elements should be in both sets
intersectionTest :: (Ord a) => Set a -> Set a -> Bool
intersectionTest (Set xs) (Set ys) = all (==True) [(z `elem` xs) && (z `elem` ys) && (not (z `elem` dif)) | z <-intersection]
                    where (Set intersection) = intersectionSet (Set xs) (Set ys)
                          (Set dif) = differenceSet (Set xs) (Set ys)

-- Simple xor function
xor :: Bool -> Bool -> Bool
xor x y = (x || y) && not (x && y)

-- Test difference
-- Elements should be in one set but not both and should be in the union of both
differenceTest :: (Ord a) => Set a -> Set a -> Bool
differenceTest (Set xs) (Set ys) = all (==True) [((z `elem` xs) `xor` (z `elem` ys)) && (z `elem` union) | z <- dif]
                where (Set union) = unionSet (Set xs) (Set ys)
                      (Set dif) = differenceSet (Set xs) (Set ys)

-- Test union function form SetOrd
-- Elements should be in either one of two sets
unionTest :: (Ord a) => Set a -> Set a -> Bool
unionTest (Set xs) (Set ys) = all (==True) [(z `elem` xs) || (z `elem` ys) | z <- union]
                    where (Set union) = unionSet (Set xs) (Set ys)

assignment3 = do
    putStrLn "Exercise 3"
    putStrLn "Test intersection"
    quickCheck (intersectionTest :: Set Int -> Set Int -> Bool)
    putStrLn "Test difference"
    quickCheck (differenceTest :: Set Int -> Set Int -> Bool)
    putStrLn "Test union"
    quickCheck (unionTest :: Set Int -> Set Int -> Bool)

-- Exercise 4 Read Chapter 5 of The Haskell Road

-- Exercise 5 20 minutes
type Rel a = [(a,a)]

-- returns the symetric closure of the relations
-- Example: [(1,2),(2,3),(3,4)] -> [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
symClos :: Ord a => Rel a -> Rel a
symClos xs = sort(xs ++ [(swap x) | x <- xs, not ((swap x) `elem` xs)])

assignment5 = do
    putStrLn "Exercise 5"
    putStrLn "Create symetric closure"
    let tr1 = [(1,2),(2,3),(3,4)]
    putStrLn $ "input -> " ++ show (tr1)
    putStrLn $ "   output -> " ++ show (symClos(tr1))
    let tr2 = [(1,1),(2,2)]
    putStrLn $ "input -> " ++ show (tr2)
    putStrLn $ "   output -> " ++ show (symClos(tr2))
    let tr3 = [(1,2),(2,1)]
    putStrLn $ "input -> " ++ show (tr3)
    putStrLn $ "   output -> " ++ show (symClos(tr3))

-- Exercise 6 40 minutes
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- returns the transitive closure of the relation
-- Example: [(1,2),(2,3),(3,4)] -> [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
-- If we only apply the transitivity once new relations dont get tested on transitivity.
-- So Reapply the function untill no new relations can be found.
-- When no new relations can be found it is fully transitive.
trClos :: Ord a => Rel a -> Rel a 
trClos xs | (nub (clos ++ (clos @@ clos)) == clos) = sort clos
          | otherwise = (trClos clos)
                where clos = nub (xs ++ (xs @@ xs))

assignment6 = do
    putStrLn "Exercise 6"
    putStrLn "Create transitive closure"
    let tr1 = [(1,2),(2,3),(3,4)]
    putStrLn $ "input -> " ++ show (tr1)
    putStrLn $ "   output -> " ++ show (trClos(tr1))
    let tr2 = [(1,1),(2,2)]
    putStrLn $ "input -> " ++ show (tr2)
    putStrLn $ "   output -> " ++ show (trClos(tr2))
    let tr3 = [(1,2),(2,1)]
    putStrLn $ "input -> " ++ show (tr3)
    putStrLn $ "   output -> " ++ show (trClos(tr3))

-- Exercise 7 60 minutes

-- Test for symmetry
-- For all if (a,b) is in the set then (b,a) should also be in the set
testSymmetry :: (Ord a) => Rel a -> Bool
testSymmetry xs = all (==True) [(swap x) `elem` sym  | x <- sym]
            where sym = (symClos xs)

-- Test for transitivity
-- First make the direct connections for each element in the set.
-- Then create a new element which links the first and second so (a,b),(b,c)->(a,c)
-- Check if the composed element is also in the list.
-- If one of these composed elements is not in the list it is not transitive.
testTransitivity :: (Ord a) => Rel a -> Bool
testTransitivity xs = all (==True) (concat [[((a,d) `elem` trans) |(c,d) <- trans,c==b] | (a,b) <- trans])
            where trans = (trClos xs)

assignment7 = do
    putStrLn "Exercise 7"
    putStrLn "Test symetry"
    quickCheck (testSymmetry :: Rel Int -> Bool)
    putStrLn "Test transitivity"
    quickCheck (testTransitivity :: Rel Int -> Bool)

-- Exercise 8
-- By generating random relations and checking if it's the same we can check the equivalence
-- If the combinations are equal the quickcheck should always pass all tests
testEquivalence :: (Ord a) => Rel a -> Bool
testEquivalence xs = trClos (symClos xs) == symClos(trClos xs)

assignment8 = do
    putStrLn "Exercise 8"
    putStrLn "This should fail"
    -- Since it does not pass all tests we can say they are not equal
    quickCheck (testEquivalence :: Rel Int -> Bool)

main = do
    print "yoyoy"
    generate arbitrary :: IO (Set Int)
    print "nog"
    genList
    quickCheck testDouble
    assignment3
    assignment5
    assignment6
    assignment7
    assignment8
    