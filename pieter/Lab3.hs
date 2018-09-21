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


-- exercise 2 120 minutes
tests = ["*(1 2)","+(2 3)","(2<=>3)","*((2<=>1) 3)"]

--samen gedaan met stephan pak ze daar even
--misschien nog even ombouwen naar unit tests dus:
--een array met tuples van alle inputs met verwachte output
--een functie die al die shit test

-- exercise 3

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
strictCnf (Dsj (z:Cnj [x,y]:[])) = Dsj [strictCnf (Cnj [x,y]),strictCnf z]
strictCnf (Cnj (z:Dsj [x,y]:[])) = Cnj [strictCnf (Dsj [x,y]),strictCnf z]

--only rewrite if its (p v (q ^ r))
strictCnf (Dsj ((Cnj (x:y:[])):z:[])) = Cnj [strictCnf (Dsj [z,x]),strictCnf (Dsj [z,y])]
strictCnf (Cnj ((Dsj (x:y:[])):z:[])) = Cnj [Dsj [x,y],z]
-- strictCnf (Cnj ((Dsj (x:y:[])):z:[])) = Dsj [strictCnf (Cnj [z,x]),strictCnf (Cnj [z,y])]

--if a conjunction or disjunction has more than 2 arguments split it
strictCnf (Cnj (x:xs)) | (length xs) > 1 = Cnj [strictCnf x,strictCnf(Cnj xs)]
                       | otherwise = Cnj (map strictCnf (x:xs))
strictCnf (Dsj (x:xs)) | (length xs) > 1 = Dsj [strictCnf x,strictCnf(Dsj xs)]
                       | otherwise = Dsj (map strictCnf (x:xs))


-- strictCnf Dsj(x:Cnj(y:ys)) = Cnj [Dsj [x,y],Dsj [x,ys]]
-- strictCnf Dsj(x:xs) = Dsj [x:Dsj xs]

-- By enforcing that all the conjunction and disjunctions are in pair and not more we
-- can ensure that the form p v (q ^ r) is always true
toPairs :: Form -> Form
toPairs (Prop x) = Prop x
toPairs (Neg f) = Neg (toPairs f)
toPairs (Cnj (x:[])) = toPairs x
toPairs (Cnj (x:xs)) | (length xs) > 1 = Cnj [toPairs x,toPairs(Cnj xs)]
                     | otherwise = Cnj (map toPairs (x:xs))
toPairs (Dsj (x:[])) = toPairs x
toPairs (Dsj (x:xs)) | (length xs) > 1 = Dsj [toPairs x,toPairs(Dsj xs)]
                     | otherwise = Dsj (map toPairs (x:xs))


testData = Dsj [p,Cnj [p,q,r],Dsj [p]]

-- test :: Form -> Form
-- test = toPairs . nnf . arrowfree

cnf :: Form -> Form
cnf = strictCnf . nnf . arrowfree


main = do
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
    print "equiv"
    print (equiv form1 form2)
    print (equiv form2 form3)
    print (equiv form3 form1)
    print "exercise 2"
    print ([parse x | x <- tests])