module Lab3 where

import Data.List
import Data.Char
import Test.QuickCheck
import Lecture3


-- Exercise 1 90 minutes

-- Testing Method
-- My method of testing this would be writing a lot of logical formulas that I know the outcome of.
-- By testing these formulas I can test my functions. It is important that formulas with different
-- lenghts and different properties are test too like p^q <-> q->(r^z)

-- contradiction :: Form -> Bool
-- contradiction form = all (\x -> not (evl x form)) (allVals form)

contradiction :: Form -> Bool
contradiction form = (length [x | x <- (allVals form),evl x form]) == 0

-- tautology :: Form -> Bool
-- tautology form = all (\x -> evl x form) (allVals form)

tautology :: Form -> Bool
tautology form = (length [x | x <- (allVals form),not (evl x form)]) == 0

-- | logical entailment 
entails :: Form -> Form -> Bool
entails frm1 frm2 = all (\x -> (evl x frm1) --> (evl x frm2)) vals
    where vals = genVals (nub ((propNames frm1) ++ (propNames frm2)))

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv frm1 frm2 = (entails frm1 frm2) && (entails frm2 frm1)

assignment1 = do
    print "exercise 1"
    print "satis"
    print (satisfiable form1)
    print (satisfiable form2)
    print (satisfiable form3)
    print "taut"
    print (tautology form1)
    print (tautology form2)
    print (tautology form3)
    print "contr"
    print (contradiction form1)
    print (contradiction form2)
    print (contradiction form3)
    print "entails"
    print (entails form1 form2)
    print (entails form2 form3)
    print (entails form3 form1)
    print "equivalence (should all be False)"
    print (equiv form1 form2)
    print (equiv form2 form3)
    print (equiv form3 form1)

-- exercise 2 120 minutes
tests = ["*(1 2)","+(2 3)","(2<=>3)","*((2<=>1) 3)"]

--samen gedaan met stephan pak ze daar even
--misschien nog even ombouwen naar unit tests dus:
--een array met tuples van alle inputs met verwachte output
--een functie die al die shit test

-- exercise 3 (200 minutes)

-- arrowfree :: Form -> Form 
-- arrowfree (Prop x) = Prop x 
-- arrowfree (Neg f) = Neg (arrowfree f)
-- arrowfree (Cnj fs) = Cnj (map arrowfree fs)
-- arrowfree (Dsj fs) = Dsj (map arrowfree fs)
-- arrowfree (Impl f1 f2) = Dsj [Neg (arrowfree f1), arrowfree f2]
-- arrowfree (Equiv f1 f2) = Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
--   where f1' = arrowfree f1
--         f2' = arrowfree f2

-- nnf :: Form -> Form 
-- nnf (Prop x) = Prop x
-- nnf (Neg (Prop x)) = Neg (Prop x)
-- nnf (Neg (Neg f)) = nnf f
-- nnf (Cnj fs) = Cnj (map nnf fs)
-- nnf (Dsj fs) = Dsj (map nnf fs)
-- nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
-- nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)

-- cnf = arrowfree nnf cnf

-- cnf :: Form-> Form
-- cnf (Prop x) = Prop x
-- cnf (Neg f) = Neg (cnf f)
-- -- cnf (Cnj (x:[])) = x
-- cnf (Cnj (x:y:[])) = Cnj [x,y]
-- cnf (Cnj (x:y:xs)) = Cnj [(Cnj [x,y]), cnf xs]

-- cnf (Dsj (x:[])) = x
-- cnf (Dsj (Conj x: xs)) = Cnj[(Dsj [cnf xs,cnf x]),Dsj [cnf xs ,cnf x]]

-- cnf (Dsj (x:xs)) = if (any (==True) [x else x | <- xs, Cnj x]) then rewrite else Dsj (x:xs)

strictCnf :: Form -> Form
strictCnf (Prop x)                 = Prop x
strictCnf (Neg x)                  = Neg (strictCnf x)


--if only one argument is given this can be simplified
strictCnf (Cnj (x:[])) = x
strictCnf (Dsj (x:[])) = x



--put disjunction and conjunction in the front
-- strictCnf (Dsj (z:Cnj [x,y]:[])) = Dsj [strictCnf (Cnj [x,y]),strictCnf z]
-- strictCnf (Cnj (z:Dsj [x,y]:[])) = Cnj [strictCnf (Dsj [x,y]),strictCnf z]

-- strictCnf (Dsj (Cnj (a:b:bs):y:ys)) = Cnj [Dsj [y,a], Dsj [y,b]]

--only rewrite if its (p v (q ^ r))
-- strictCnf (Dsj ((Cnj (x:y:[])):z:[])) = Cnj [strictCnf (Dsj [z,x]),strictCnf (Dsj [z,y])]
-- --already correct
-- strictCnf (Cnj ((Dsj (x:y:[])):z:[])) = Cnj [Dsj [x,y],z]
-- -- strictCnf (Cnj ((Dsj (x:y:[])):z:[])) = Dsj [strictCnf (Cnj [z,x]),strictCnf (Cnj [z,y])]


strictCnf (Dsj [Cnj [x,y],z]) = Cnj [strictCnf(Dsj [z,x]),strictCnf(Dsj [z,y])]
strictCnf (Dsj [z, Cnj[x,y]]) = Cnj [strictCnf(Dsj [z,x]),strictCnf(Dsj [z,y])]

strictCnf (Dsj xs) = Dsj (map strictCnf xs)
strictCnf (Cnj xs) = Cnj (map strictCnf xs)

--if a conjunction or disjunction has more than 2 arguments split it
-- strictCnf (Cnj (x:xs)) | (length xs) > 1 = Cnj [strictCnf x,strictCnf(Cnj xs)]
--                        | otherwise = Cnj (map strictCnf (x:xs))
-- strictCnf (Dsj (x:xs)) | (length xs) > 1 = Dsj [strictCnf x,strictCnf(Dsj xs)]
--                        | otherwise = Dsj (map strictCnf (x:xs))


-- strictCnf Dsj(x:Cnj(y:ys)) = Cnj [Dsj [x,y],Dsj [x,ys]]
-- strictCnf Dsj(x:xs) = Dsj [x:Dsj xs]

-- By enforcing that all the conjunction and disjunctions are in pair and not more we
-- can ensure that the form p v (q ^ r) is always true
-- If we dont this the functions crashed on inputs like Cnj [p,q,r]
toPairs :: Form -> Form
toPairs (Prop x) = Prop x
toPairs (Neg x) = Neg (toPairs x)
toPairs (Cnj (x:[])) = toPairs x
toPairs (Cnj (x:xs)) | (length xs) > 1 = Cnj [toPairs x,toPairs(Cnj xs)]
                     | otherwise = Cnj (map toPairs (x:xs))
toPairs (Dsj (x:[])) = toPairs x
toPairs (Dsj (x:xs)) | (length xs) > 1 = Dsj [toPairs x,toPairs(Dsj xs)]
                     | otherwise = Dsj (map toPairs (x:xs))


td1 = Dsj [p,Cnj [p,q,r],Dsj [p]]
td2 = Cnj [p, Dsj [p,q,r], Cnj [p]]

-- test :: Form -> Form
-- test = toPairs . nnf . arrowfree

-- just reapplying untill it is in the correct format is key.
-- if we do this we just look for the pattern
-- when not doing this the algorithm isnt that good in circling back up
-- so one you replace something in the 'bottom' the 'top' of the tree needs
-- to be computed again
cnf :: Form -> Form
cnf frm = while (not . isCnf) strictCnf inp
      where inp = toPairs (nnf (arrowfree frm))

-- Check if the function is in cnf form.
isCnf :: Form -> Bool
isCnf (Prop x) = True
isCnf (Neg (Prop x)) = True
isCnf (Neg _) = False
isCnf (Dsj xs) = not (any isCnj xs) && (all (==True) (map isCnf xs))
isCnf (Cnj xs) = all (==True) (map isCnf xs)
isCnf (Impl x y) = False
isCnf (Equiv x y) = False

-- Because appearantly we cant do list comprehension and do ==Cnj we add this fucntion.
isCnj :: Form -> Bool
isCnj (Cnj xs) = True
isCnj _ = False

assignment3 = do
    print "exercise 3"
    print "Check equivalence with original"
    print (equiv (cnf form1) form1)
    print (equiv (cnf form2) form2)
    print (equiv (cnf form3) form3)
    print (equiv (cnf td1) td1)
    print (equiv (cnf td2) td2)
    print "reapplying cnf on an already cnf should do nothing"
    print (cnf form1 == (cnf (cnf form1)))
    print (cnf form2 == (cnf (cnf form2)))
    print (cnf form3 == (cnf (cnf form3)))


-- Exercise 4
--https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators
--https://www.fpcomplete.com/blog/2017/01/quickcheck
-- generate arbitrary :: IO Form
instance Arbitrary Form where
    -- arbitrary :: Gen Form
    arbitrary = do
        n_operators <- choose(1,10)
        -- arr <- [(Prop x) | x <- [0..n_operators]]
        -- oneof [return Prop x, return Neg ]
        --make an array of n operators
        --then choose a operator for each array index
        --generate random atom around the operators
        --randomly make the atom negative.
        return (Cnj [Prop n_operators,Prop 1])



-- genForm :: Gen Form
-- genForm = Cnj [Prop (choose (1,10)),Prop (choose (1,10))]
-- class Arbitrary a where
--     arbitrary :: Gen a


main = do
    assignment1
    print "exercise 2"
    print ([parse x | x <- tests])
    assignment3