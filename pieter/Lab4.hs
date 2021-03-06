module Lab4 where

import Data.List
import Data.Char
import Data.Tuple
import Test.QuickCheck
import SetOrd
import System.Random
import Lecture4

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
    length <- (randomRIO (0,50))
    xs <- (randomList length)
    return (Set (sort (nub xs)))

-- credit https://stackoverflow.com/a/30741139
randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (-100,100)
  rs <- randomList (n-1)
  return (r:rs)   

-- simple test functions to see if quickcheck works
doubleList :: Set Int -> Set Int
doubleList (Set xs) = Set [x*2 | x <- xs]

testDouble :: Set Int -> Bool
testDouble (Set xs) = (doubleList (Set xs)) == Set (map (*2) xs)

myGen :: (Set Int -> Bool) -> Int -> IO [Char]
myGen f count = do
    list <-genList
    nextTest <- if count > 0 then myGen f (count - 1) else return ("Test failed " ++ show list)
    return (if (f list) then "All tests passed" else nextTest)

ownQuickCheck :: (Set Int -> Bool) -> IO [Char]
ownQuickCheck f = myGen f 100

-- testOwnGenerator = do
--     list <- genList
--     let tested = ((doubleList list) == (function list))
--     if tested
--         then
--             putStrLn $ "test passed" ++ (show list)
--         else
--             putStrLn $ "test failed" ++ (show list)
--     return tested
--     where function = (\(Set xs) -> Set (map (*2) xs))

-- ownQuickCheck fun = do
--     x <- (sequence [fun | x <- [0..99]])
--     if (all (==True) x) 
--         then print "All tests Passed" 
--         else print "Not all passed"



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
-- Read but no real questions about it


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
testTransitivity xs = clause1 && clause2
            where trans = (trClos xs)
                  -- Standard clause for checking if all the elements transitively trace back
                  clause1 = all (==True) (concat [[((a,d) `elem` trans) |(c,d) <- trans,c==b] | (a,b) <- trans])
                  -- Do some other checks to test that not too much elements are present (not perfect yet)
                  clause2 = all (==True) [(a `elem` seconds) || (b `elem` firsts) || ((a `elem` firsts) && (b `elem` seconds)) |(a,b) <- trans]
                  firsts = [x | (x,_) <- xs]
                  seconds = [x | (_,x) <- xs]


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

-- Bonus exercise
-- Remove Show from the declerations in the Lecture4 class

-- Overwrite show for expressions
instance Show Expr where
    show (I x) = show x
    show (V x) = show x
    show (Add e1 e2) = "("++show e1 ++ " + " ++ show e2++")"
    show (Subtr e1 e2) = "("++show e1 ++ " - " ++ show e2++")"
    show (Mult e1 e2) = "("++show e1 ++ " * " ++ show e2++")"

-- Overwrite show for conditions
instance Show Condition where
    show (Prp v) = (show v)
    show (Eq e1 e2) = "("++(show e1)++" == "++(show e2)++")"
    show (Lt e1 e2) = "("++(show e1)++" < "++(show e2)++")"
    show (Gt e1 e2) = "("++(show e1)++" > "++(show e2)++")"
    show (Ng e) = "(-"++(show e)++")"
    -- Looks weird but the head and tail are for preventing trailing operators
    show (Cj xs) = "("++concat ([show(head xs)]++[" ^ "++(show x) | x <- (tail xs)])++")"
    show (Dj xs) = "("++concat ([show(head xs)]++[" v "++(show x) | x <- (tail xs)])++")"
    
-- Overwrite show for statements
instance Show Statement where
    show (Ass v e) = (show v) ++ " = " ++ (show e)
    -- Add some spaces to make the the clauses super fancy looking
    show (Cond c s1 s2) = "if "++(show c)++" then\n    "++(show s1)++"\nelse\n    "++(show s2)
    -- Head and tail is for preventing trailing newline
    show (Seq xs) = concat ([show (head xs)]++["\n"++(show x) | x <-tail xs])
    show (While c s) = "while "++(show c)++" then\n    "++(show s)

assignmentBonus = do
    -- Declare some test variables
    let e1 = I 10
    let e2 = V "foo"
    let e3 = Add e1 e2
    let e4 = Subtr e1 e2
    let e5 = Mult e1 e2

    let p1 = Prp "p1"
    let p2 = Prp "p2"

    let c1 = p1
    let c2 = Eq e3 e2
    let c3 = Lt e3 e2
    let c4 = Gt e3 e2
    let c5 = Cj [p1,p2,p1,Ng p2]
    let c6 = Dj [p1,p2,p1,Ng p2]

    let s1 = Ass "bar" e3
    let s2 = Ass "boo" e1
    let s3 = Cond c2 s1 s2
    let s4 = While c1 s2
    let s5 = Seq [s1,s2,s3,s4]

    putStrLn "Bonus exercise"
    putStrLn "Expressions"    
    sequence (map print [e1,e2,e3,e4,e5])
    putStrLn "Conditions"
    sequence (map print [c1,c2,c3,c4,c5,c6])
    putStrLn "Statements"
    print s5

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
    assignmentBonus