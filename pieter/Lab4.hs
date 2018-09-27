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
instance Arbitrary a => Arbitrary (Set a) where
    arbitrary = do
        xs <- arbitrary
        return (Set xs)

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

-- Exercise 3 upto now like 30 minutes

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

-- Exercise 4 Read Chapter 5 of The Haskell Road

-- Exercise 5 20 minutes
type Rel a = [(a,a)]

-- returns the symetric closure of the relations
-- Example: [(1,2),(2,3),(3,4)] -> [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
symClos :: Ord a => Rel a -> Rel a
symClos xs = xs ++ [(swap x) | x <- xs, not ((swap x) `elem` xs)]

assignment5 = do
    putStrLn "exercise 5"
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
trClos xs | (nub (clos ++ (clos @@ clos)) == clos) = clos
          | otherwise = (trClos clos)
                where clos = nub (xs ++ (xs @@ xs))

assignment6 = do
    putStrLn "exercise 6"
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


main = do
    print "yoyoy"
    generate arbitrary :: IO (Set Int)
    print "nog"
    genList
    quickCheck testDouble

    assignment6
    