module Lab3 where
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- 1.

{-
    contradiction :: Form -> Bool

    tautology :: Form -> Bool

    -- | logical entailment 
    entails :: Form -> Form -> Bool

    -- | logical equivalence
    equiv :: Form -> Form -> Bool
-}

tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = all (\v -> evl v f == False) (allVals f)

testTaut,testCon :: Form
testTaut = Dsj [Prop 1, Neg (Prop 1)]
testCon = Cnj [Prop 1, Neg (Prop 1)]

assignment1 = do
    print "Assignment 1"
    print (tautology testTaut)
    print (tautology testCon)
    print (contradiction testCon)
    print (contradiction testTaut)


main = do
    assignment1