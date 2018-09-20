module Lab3 where
 
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- 1. Giving definitions. Time: 80 min
contradiction :: Form -> Bool
contradiction f = not(satisfiable f)
-- SIMPLE TEST: contradiction (Cnj [p, Neg p])

tautology :: Form -> Bool
tautology f = all(\ v -> evl v f) (allVals f)
-- SIMPLE TEST: tautology  (Dsj [p, Neg p])

-- | logical entailment 
entails :: Form -> Form -> Bool
entails f1 f2 = all(\v -> (evl v f1) --> (evl v f2)) (genVals(nub(propNames f1 ++ propNames f2)))

-- -- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = (entails f1 f2) && (entails f2 f1)
-- SIMPLE TEST: equiv form3 form1, equiv form1 form3

    -- Check that your definitions are correct.
    -- Deliverables: implementation, description of your method of checking the definitions, indication of time spent.

-- 2. Test the parse function for parsing propositional formulas. Time: ...

    -- Deliverables: test report describing the test method used and the outcome of the test, indication of time spent.

-- 3. Haskell program for converting formulas into CNF. Time: ...

    -- Deliverables: conversion program with documentation, indication of time spent.

-- 4. (...). Time: ...

    -- Deliverables: generator for formulas, sequence of test properties, test report, indication of time spent.

-- 5. Bonus. Time: ...

    -- Deliverables: Conversion program, test generator, test properties, documentation of the automated testing process. Also, give an indication of time spent.