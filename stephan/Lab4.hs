
module Lab4
where 
import Data.List
import System.Random
import Test.QuickCheck
import Lecture4
import SetOrd

-- 1 2 hours

{-
    Did not know this: A − B = { x | x ∈ A ∧ x 6∈ B } their difference.

    Useful: A 6⊆ B ⇔ A − B 6 = ∅
            A ∩ B = A − (A − B)
    
    A i is the collection of all sets. This last fact is an example of a
    trivially true implication: if I = ∅, then every statement i ∈ I is 
    false, hence the implication i ∈ I ⇒ x ∈ A i true
-}

-- 2 Time: 1.5 Hours
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

-- Some help from:
-- http://geekyplatypus.com/y-u-have-no-code-samples-quickcheck/
instance (Arbitrary a, Ord a, Eq a) => Arbitrary (Set a) where
    arbitrary = do
                list <- arbitrary
                return $ Set (quickSort list)

-- Create a random Set Int
randomIntSet :: IO (Set Int)
randomIntSet = generate arbitrary :: IO (Set Int)

-- Checks if Set contains duplicate
containsDupSet :: (Ord a) => Set a -> Bool
containsDupSet (Set []) = True
containsDupSet (Set (x:xs)) = (not (inSet x (Set xs))) && 
    (containsDupSet (Set xs))

-- Checks if Set is ordered
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


-- 3 Time: 1 hour + 30 min
{-
    Implement operations for set intersection, set union and set 
    difference, for the datatype Set defined in SetOrd.hs. Next, 
    use automated testing to check that your implementation is correct.
    First use your own generator, next use QuickCheck.
-}

intersectionSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set xs) set = Set [x | x <- xs, inSet x set]

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set xs) (Set ys) = Set (xs \\ ys)

setToList :: Set a -> [a]
setToList (Set xs) = xs

-- https://annevankesteren.nl/2007/02/haskell-xor 
xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a

-- From the union of set1 and set2 and intersection set every element is one of the following;
-- 1) in the intersection and in set1 and in set2
-- 2) not in the intersection and Not in set1
-- 3) not in the intersection and Not in set2
checkIntersectionSet  :: (Ord a) => Set a -> Set a -> Bool
checkIntersectionSet set1 set2 = all (\x -> x) (map check union)
    where 
        union = setToList (unionSet (unionSet set1 set2) intersection)
        check = (\x -> xor (xor (inSet x intersection && inSet x set1 && inSet x set2) 
                    (not (inSet x intersection) && not (inSet x set1)))
                    (not (inSet x intersection) && not (inSet x set2)))
        intersection = intersectionSet set1 set2 

-- From the union of set1 and set2 and the difference set every element is one of the following;
-- 1) x E set1 ^ x -E set2 ^ x E diff
-- 2) x E set1 ^ x E set2 ^ x -E diff
-- 3) x -E set1 ^ x E set2 ^ x -E diff
checkDifferenceSet  :: (Ord a) => Set a -> Set a -> Bool
checkDifferenceSet set1 set2 = all (\x -> x) (map check union)
    where 
        union = setToList (unionSet (unionSet set1 set2) diff)
        check = (\x -> ((inSet x set1) && (not (inSet x set2)) && (inSet x diff)) || 
                    ((inSet x set1) && (inSet x set2) && (not (inSet x diff))) ||
                    ((not (inSet x set1)) && (inSet x set2) && (not (inSet x diff))))
        diff = differenceSet set1 set2 

-- Check if the concatination of the Set is the same as the union
checkUnionSet :: (Ord a) => Set a -> Set a -> Bool
checkUnionSet (Set xs) (Set ys) = setToList (unionSet (Set xs) (Set ys)) 
    == sort (nub (xs ++ ys))


assignment3 = do
    print ("Assingment 3")
    quickCheck (checkIntersectionSet :: Set Int -> Set Int -> Bool)
    quickCheck (checkDifferenceSet :: Set Int -> Set Int -> Bool)
    quickCheck (checkUnionSet :: Set Int -> Set Int -> Bool)


-- 4

{-
    Definition 5.1 (Relations, Domain, Range) A relation is a set of 
    ordered pairs. Instead of (x, y) ∈ R — where R is a relation — one 
    usually writes xRy, or R(x, y), or Rxy. The set dom (R) = 
    {x | ∃y ( xRy )}, i.e., the set consisting of all first coordinates
    of pairs in R, is called the domain of R and ran(R) = {y | ∃x ( xRy )},
    the set of second coordinates of pairs in R, its range.

    Definition 5.3 (From . . . to, Between, On) The relation R is a 
    relation from A to B or between A and B, if dom (R) ⊆ A and ran(R) ⊆ B.
    A relation from A to A is called on A.

    reflexive on A if for every x ∈ A: xRx.
    irreflexive if for no x ∈ A: xRx.
    symmetric if for all x, y ∈ A: if xRy then yRx.
    asymmetric if for all x, y ∈ A: if xRy then not yRx.
    antisymmetric if for all x, y ∈ A: if xRy and yRx then x = y
    transitive if for all x, y, z ∈ A: if xRy and yRz then xRz.
    intransitive if for all x, y, z ∈ A: if xRy and yRz then not xRz.
    pre-order (or quasi-order) if R is transitive and reflexive.
    strict partial order if R is transitive and irreflexive
    partial order if R is transitive, reflexive and antisymmetric.
    linear (or: has the comparison property) if for all x, y ∈ A: xRy or yRx or x = y.
    
    reflexivity     ∀x xRx.
    irreflexivity   ∀x ¬xRx.
    symmetry        ∀xy (xRy ⇒ yRx).
    asymmetry       ∀xy (xRy ⇒ ¬yRx).
    antisymmetry    ∀xy (xRy ∧ yRx ⇒ x = y).
    transitivity    ∀xyz (xRy ∧ yRz ⇒ xRz).
    intransitivity  ∀xyz (xRy ∧ yRz ⇒ ¬xRz).
    linearity       ∀xy (xRy ∨ yRx ∨ x = y)

-}



-- 5 Time: 10 min

{-
    Suppose we implement binary relations as list of pairs, Haskell 
    type [(a,a)]. Assume the following definition:

    > type Rel a = [(a,a)]

    Use this to implement a function

    symClos :: Ord a => Rel a -> Rel a

    that gives the symmetric closure of a relation, where the relation 
    is represented as an ordered list of pairs. E.g., symClos 
    [(1,2),(2,3),(3,4)] should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)].
-}

type Rel a = [(a,a)]

-- If (x,y) ∈ A then (y,x) ∈ A
symClos :: Ord a => Rel a -> Rel a
symClos xs = sort (nub (xs ++ [(y,x)| (x,y) <- xs]))

assignment5 = do
    print ("Assingment 5")
    print (symClos [(1,2),(2,3),(3,4)] == [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)])

-- 6 Time: 20 min

{-

    Use the datatype for relations from the previous exercise, plus

    > infixr 5 @@
    > 
    > (@@) :: Eq a => Rel a -> Rel a -> Rel a
    > r @@ s = 
    >   nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

    to define a function

    trClos :: Ord a => Rel a -> Rel a 

    that gives the transitive closure of a relation, represented as an 
    ordered list of pairs. E.g., trClos [(1,2),(2,3),(3,4)] should give
    [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].
-}

infixr 5 @@ 

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- if (x,y) ∈ A ^ (y,z) ∈ A -> (x,z) ∈ A
trClos :: Ord a => Rel a -> Rel a 
trClos = fp (\xs -> sort (nub (xs ++ xs @@ xs)))

assignment6 = do
    print ("Assignment 6")
    print (trClos [(1,2),(2,3),(3,4)] == [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)])

-- 7 time: 1 hour + 30 minuts

{-
    Test the functions symClos and trClos from the previous exercises. 
    Devise your own test method for this. Try to use random test generation. 
    Define reasonable properties to test. Can you use QuickCheck? How?

    (Deliverables: test code, short test report, indication of time spent.)
-}

-- For every (x,y) ∈ A -> (y,x) ∈ A
checkSymClos :: Ord a => Rel a -> Bool
checkSymClos xs = all (\x -> x) (map (\(x,y) -> elem (y,x) symclos) symclos)
    where
        symclos = symClos xs

-- returns all [(x,_)] in xs
startingWith :: (Eq a) => a-> Rel a -> Rel a
startingWith x xs = [(a,b)| (a,b) <- xs, a == x]

endingWith :: (Eq a) => a-> Rel a -> Rel a
endingWith x xs = [(a,b)| (a,b) <- xs, b == x]

-- this is always the smallest closure
-- (R U R*R U R^2*R U R^3*R) until R^n = EmptySet
smallestClose :: Ord a => Rel a -> Rel a -> Rel a 
smallestClose [] _ = []
smallestClose xs ys | (xs @@ ys) \\ xs == [] = xs
                    | otherwise = nub (sort (xs ++ (smallestClose (xs @@ ys) ys)))


-- If (a,b) ∈ A ^ (b,c) ∈ A -> (a,c) ∈ A
-- Checks if smallest closure
checktrClos :: Ord a => Rel a -> Bool
checktrClos xs = all (==True) ([elem (a,d) trclos| (a,b) <- trclos, 
                    (c,d) <- trclos, b == c] ++
                    [not (null (startingWith a xs)) && not (null (endingWith b xs))| (a,b) <- trclos])
                    && (smallestClose xs xs == trclos)               
                    where 
                        trclos = trClos xs


assignment7 = do
    print ("Assingment 7")
    quickCheck (checkSymClos :: Rel Int -> Bool)
    quickCheck (checktrClos :: Rel Int -> Bool)

-- 8

{-
    Is there a difference between the symmetric closure of
    the transitive closure of a relation R and the transitive 
    closure of the symmetric closure of R?

    Deliverable: If your answer is that these are the same, you should 
    give an argument, if you think these are different you should give 
    an example that illustrates the difference.
-}

-- Check wether the sequence matters
testClosure :: (Eq a, Ord a) => Rel a -> Bool
testClosure xs = trClos (symClos xs) == symClos (trClos xs)


assignment8 = do
    print ("Assingment 8")
    print ("yes it matters Example Rel [(0,1)]")
    print (trClos (symClos ([(0,1)])))
    print (symClos (trClos ([(0,1)])))
    quickCheck (testClosure :: Rel Int -> Bool)


main = do
    assignment2
    assignment3
    assignment5
    assignment6
    assignment7
    assignment8