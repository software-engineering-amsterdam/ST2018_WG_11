
module Lab4
where 
import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck
import Lecture4
import SetOrd

-- 1

{-
    Pretty much everything was clear but some exercises and
    examples were vague like exercise 4.33 and 4.34

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
    - Did not know this: A − B = { x | x ∈ A ∧ x 6∈ B } their difference.
    - Useful: A 6⊆ B ⇔ A − B 6 = ∅
              A ∩ B = A − (A − B)
    - A i is the collection of all sets. This last fact is an example of a
      trivially true implication: if I = ∅, then every statement i ∈ I is 
      false, hence the implication i ∈ I ⇒ x ∈ A i true
-}

-- 2
{-
    Implement a random data generator for the datatype Set Int,
    where Set is as defined in SetOrd.hs. First do this from 
    scratch, next give a version that uses QuickCheck to random 
    test this datatype.
-}

-- Generate list of random integers sorted and withouth duplicates
-- Use this function to determine length of the list
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

-- Generate random input and test this n times
myGen :: (Set Int -> Bool) -> Int -> IO [Char]
myGen f count = do
    list <-genList
    nextTest <- if count > 0 then myGen f (count - 1) else return ("Test failed " ++ show list)
    return (if (f list) then "All tests passed" else nextTest)

-- mimic quickcheck functionality
ownQuickCheck :: (Set Int -> Bool) -> IO [Char]
ownQuickCheck f = myGen f 100


-- Some help from:
-- http://geekyplatypus.com/y-u-have-no-code-samples-quickcheck/
instance (Arbitrary a, Ord a, Eq a) => Arbitrary (Set a) where
    arbitrary = do
                list <- arbitrary
                return $ Set (sort (nub list))

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


-- 3
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
checkUnionSet (Set xs) (Set ys) = p1 && p2
    where 
        p1 = setToList (unionSet (Set xs) (Set ys)) == sort (nub (xs ++ ys)) 
        p2 = all (==True) [(z `elem` xs) || (z `elem` ys) | z <- union]
            where (Set union) = unionSet (Set xs) (Set ys)


assignment3 = do
    print ("Assingment 3")
    putStrLn "Test intersection"
    quickCheck (checkIntersectionSet :: Set Int -> Set Int -> Bool)
    putStrLn "Test difference"
    quickCheck (checkDifferenceSet :: Set Int -> Set Int -> Bool)
    putStrLn "Test union"
    quickCheck (checkUnionSet :: Set Int -> Set Int -> Bool)


-- 4

{-
    1. "Equivalence relations on a set A enable us to partition the set A into equivalence classes" (page 193)
       (and Definition 5.75 etc) not very clear. What do they mean? 5.7 gives an impression, but the definition
       is still quite abstract.

    Keep in mind:
    - Definition 5.1 (Relations, Domain, Range) A relation is a set of 
    ordered pairs. Instead of (x, y) ∈ R — where R is a relation — one 
    usually writes xRy, or R(x, y), or Rxy. The set dom (R) = 
    {x | ∃y ( xRy )}, i.e., the set consisting of all first coordinates
    of pairs in R, is called the domain of R and ran(R) = {y | ∃x ( xRy )},
    the set of second coordinates of pairs in R, its range.

    - Definition 5.3 (From . . . to, Between, On) The relation R is a 
    relation from A to B or between A and B, if dom (R) ⊆ A and ran(R) ⊆ B.
    A relation from A to A is called on A.

    - reflexive on A if for every x ∈ A: xRx.
    - irreflexive if for no x ∈ A: xRx.
    - symmetric if for all x, y ∈ A: if xRy then yRx.
    - asymmetric if for all x, y ∈ A: if xRy then not yRx.
    - antisymmetric if for all x, y ∈ A: if xRy and yRx then x = y
    - transitive if for all x, y, z ∈ A: if xRy and yRz then xRz.
    - intransitive if for all x, y, z ∈ A: if xRy and yRz then not xRz.
    - pre-order (or quasi-order) if R is transitive and reflexive.
    - strict partial order if R is transitive and irreflexive
    - partial order if R is transitive, reflexive and antisymmetric.
    - linear (or: has the comparison property) if for all x, y ∈ A: xRy or yRx or x = y.
    
    - reflexivity     ∀x xRx.
    - irreflexivity   ∀x ¬xRx.
    - symmetry        ∀xy (xRy ⇒ yRx)
    - asymmetry       ∀xy (xRy ⇒ ¬yRx)
    - antisymmetry    ∀xy (xRy ∧ yRx ⇒ x = y)
    - transitivity    ∀xyz (xRy ∧ yRz ⇒ xRz)
    - intransitivity  ∀xyz (xRy ∧ yRz ⇒ ¬xRz)
    - linearity       ∀xy (xRy ∨ yRx ∨ x = y)
-}


-- 5

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
symClos xs = sort(xs ++ [(swap x) | x <- xs, not ((swap x) `elem` xs)])

assignment5 = do
    print ("Assingment 5")
    print (symClos [(1,2),(2,3),(3,4)] == [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)])
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

-- 6

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

-- 7

{-
    Test the functions symClos and trClos from the previous exercises. 
    Devise your own test method for this. Try to use random test generation. 
    Define reasonable properties to test. Can you use QuickCheck? How?

    (Deliverables: test code, short test report, indication of time spent.)
-}

-- For every (x,y) ∈ A -> (y,x) ∈ A
testSymmetry :: Ord a => Rel a -> Bool
testSymmetry xs = all (==True) [(swap x) `elem` sym  | x <- sym]
            where sym = (symClos xs)

-- First test: 
-- If (a,b) ∈ A ^ (b,c) ∈ A -> (a,c) ∈ A
-- Second test:
-- If it is the smallest transitive closure
testTransitivity :: Ord a => Rel a -> Bool
testTransitivity xs = all (==True) ([elem (a,d) trclos| (a,b) <- trclos, 
                    (c,d) <- trclos, b == c]) &&
                    smallestClosesure xs == trclos
                    where 
                        trclos = trClos xs
-- TODO does not check for smallest transitive closure yet

-- Create smallest transitive closure
smallestClosesure :: Ord a => Rel a -> Rel a
smallestClosesure xs = smallestClose xs xs []

-- (R U R*R U R^2*R U R^3*R .. U R^n) 
-- until U(R^(n+1)) // U (R^n) = EmptySet
smallestClose :: Ord a => Rel a -> Rel a -> Rel a -> Rel a 
smallestClose [] _ u = nub (sort u)
smallestClose rn r u | (rn @@ r) \\ u == [] = nub (sort (u ++ rn))
                     | otherwise = smallestClose (rn @@ r) r (u++rn)

assignment7 = do
    print ("Assingment 7")
    putStrLn "Test symetry"
    quickCheck (testSymmetry :: Rel Int -> Bool)
    putStrLn "Test transitivity"
    quickCheck (testTransitivity :: Rel Int -> Bool)

-- 8

{-
    Is there a difference between the symmetric closure of
    the transitive closure of a relation R and the transitive 
    closure of the symmetric closure of R?

    Deliverable: If your answer is that these are the same, you should 
    give an argument, if you think these are different you should give 
    an example that illustrates the difference.
-}

-- By generating random relations and checking if it's the same we can check the equivalence
-- If the combinations are equal the quickcheck should always pass all tests
testEquivalence :: (Ord a) => Rel a -> Bool
testEquivalence xs = trClos (symClos xs) == symClos(trClos xs)


assignment8 = do
    print ("Assingment 8")
    print ("yes it matters Example Rel [(0,1)]")
    putStrLn "Transitivity (Symmetry xs)"
    print (trClos (symClos ([(0,1)])))
    putStrLn "Symmetry (Transitivity xs)"
    print (symClos (trClos ([(0,1)])))
    putStrLn "QuickCheck should fail"
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
    assignment2
    assignment3
    assignment5
    assignment6
    assignment7
    assignment8
    assignmentBonus