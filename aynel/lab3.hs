module Lab3 where
 
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
-- import Control.Monad.Random

-- 1. Giving definitions. Time: 100 min
contradiction :: Form -> Bool
contradiction f = not(satisfiable f)

tautology :: Form -> Bool
tautology f = all(\ v -> evl v f) (allVals f)

-- | logical entailment 
entails :: Form -> Form -> Bool
entails f1 f2 = all(\v -> (evl v f1) --> (evl v f2)) (genVals(nub(propNames f1 ++ propNames f2)))
-- test!!

-- -- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = (entails f1 f2) && (entails f2 f1)


    -- Deliverables: description of your method of checking the definitions

testCont, testTaut1, testTaut2 :: Form
testCont = (Cnj [p, Neg p])
testTaut1 = (Dsj [p, Neg p])
testTaut2 = (Dsj [Neg q, q])


testDefs = do
    print "Testing contradiction"
    print (contradiction testCont)

    print "Testing tautology"
    print (tautology testTaut1)
    print (tautology testTaut2)

    print "Testing logical equivalence"
    print (equiv form3 form1)
    print (equiv form1 form3)

    print "Testing logical entailment"



-- 2. Test the parse function for parsing propositional formulas. Time: ... (18:00 begonnen)

parseConj1 = parse "*(1 -1)"
parseConj2 = parse "*(2 3)"
-- "*(2- 3)" includen in test

-- parseTauto1 = parse ""
-- parseTauto2 = parse ""

-- parseMultDsj1 = parse "+ (1 1 1 1 1)"
-- parseMultDsj2 = parse "+ (1 2 3 4 5)"
-- parseSingleDsj = parse "+ (1)" 

-- parseImpl = parse ""
-- parseEquiv = parse ""

-- first should not work, second should give back beginning
parseUnknown1 = "&(1 1)"
-- expected output = *(1 1)
parseUnknown2 = "*(1 1) asdfghj!@#$%^&*("

-- impl
-- equiv
-- +(2 -3)
-- parse "*(+(* (1 1))))) "

testParser = do
    -- parse conjunctions
    print "Test conjunctions: should be True"
    -- print ((parseConj1 !! 0) == "*(1 -1)")
    -- print (parseConj1 !! 0 == "* ()")

    -- Deliverables: test report describing the test method used and the outcome of the test, indication of time spent.

-- 3. Haskell program for converting formulas into CNF. Time: ...

    -- Deliverables: conversion program with documentation, indication of time spent.

-- 4. Generator. Time: ... 13:20
operators = ["+", "*", "-", "==>", "<=>"]
props = [1..100]

-- selectOP :: Int -> IO Int
-- selectOP n = do x <- randomRIO(0, length operators)
--                 return []
--                 -- return x

-- adjust chance of proposition with layer
functie :: Int -> Form
functie layer | randomInt > 0 && randomInt <= 60 = (Prop propVal)
              | randomInt > 60 && randomInt <= 70 = Dsj [functie layer, functie layer]
              | randomInt > 70 && randomInt <= 80 = Cnj[functie layer,functie layer]
              | randomInt > 80 && randomInt <= 90 = Impl (functie layer) (functie layer)
              | randomInt > 90 && randomInt <= 100 = Equiv (functie layer) (functie layer)
              where intMax = 100
                    randomInt <- choose (1,intMax)
                    propVal <- choose(1,10)



    -- Deliverables: generator for formulas, sequence of test properties, test report, indication of time spent.

-- 5. Bonus. Time: ...

    -- Deliverables: Conversion program, test generator, test properties, documentation of the automated testing process. Also, give an indication of time spent.