module Lab4 where

import Data.List
import Data.Char
import Test.QuickCheck
import SetOrd
import System.Random

-- Exercise 1 Read Chapter 4 of The Haskell Road
-- 90 minutes
-- Pretty much everything was clear but some exercises and 
-- examples were vague like exercise 4.33 and 4.34

-- Exercise 4 Read Chapter 5 of The Haskell Road

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

-- unionSet :: (Ord a) => Set a -> Set a -> Set a 
-- unionSet (Set [])     set2  =  set2
-- unionSet (Set (x:xs)) set2  = 
--    insertSet x (unionSet (Set xs) set2)

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


main = do
    print "yoyoy"
    generate arbitrary :: IO (Set Int)
    print "nog"
    genList
    quickCheck testDouble
    