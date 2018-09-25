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

-- 4. Generator.

-- 5. Bonus.