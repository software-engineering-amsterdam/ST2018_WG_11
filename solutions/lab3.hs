module Lab3 where
 
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- 1. Giving definitions.
contradiction :: Form -> Bool
contradiction f = not(satisfiable f)

tautology :: Form -> Bool
tautology f = all(\ v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails frm1 frm2 = all (\x -> (evl x frm1) --> (evl x frm2)) vals
    where vals = genVals (nub ((propNames frm1) ++ (propNames frm2)))

equiv :: Form -> Form -> Bool
equiv f1 f2 = (entails f1 f2) && (entails f2 f1)


-- deze is van Pieterke; welke houden we erin? CHECKEN :D mijne doet wel vergelijkbare dingen
assignment1 = do
    print "exercise 1"
    print "satisfiable"
    print (satisfiable form1)
    print (satisfiable form2)
    print (satisfiable form3)
    print "tautology"
    print (tautology form1)
    print (tautology form2)
    print (tautology form3)
    print "contradiction"
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

-- (Deliverables: description of your method of checking the definitions)

-- 2. Test the parse function for parsing propositional formulas.

-- 3. Haskell program for converting formulas into CNF.
strictCnf :: Form -> Form
strictCnf (Prop x)                 = Prop x
strictCnf (Neg x)                  = Neg (strictCnf x)

--if only one argument is given this can be simplified
strictCnf (Cnj (x:[])) = x
strictCnf (Dsj (x:[])) = x

-- Replace the (p v (q ^ r)) with ((p v q) ^ (p v r))
strictCnf (Dsj [Cnj [x,y],z]) = Cnj [strictCnf(Dsj [z,x]),strictCnf(Dsj [z,y])]
strictCnf (Dsj [z, Cnj[x,y]]) = Cnj [strictCnf(Dsj [z,x]),strictCnf(Dsj [z,y])]

-- Check every element
strictCnf (Dsj xs) = Dsj (map strictCnf xs)
strictCnf (Cnj xs) = Cnj (map strictCnf xs)

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
-- 4. Generator.

-- 5. Bonus.