module Lab3 where
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- 1.

tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = all (\v -> evl v f == False) (allVals f)

-- Generate all possible valuations for two formula's
allVals2Forms :: Form -> Form -> [Valuation]
allVals2Forms f1 f2 = genVals (nub ((propNames f1) ++ (propNames f2)))

-- f1 implies f2
entails :: Form -> Form -> Bool
entails f1 f2 = all (\v -> ((evl v f1) --> (evl v f2))) (allVals2Forms f1 f2)

-- f1 equivalance with f2
equiv :: Form -> Form -> Bool
equiv f1 f2 = entails f1 f2 && entails f2 f1

{-
    Tested tautology with:
        p v not p ==> True
    Tested contradiction with:
        p ^ not p ==> True
    Tested entails with:
        p v not p -> p v not p ==> True
        p v not p -> f v not f ==> True
        p ^ not p -> f ^ not f ==> True
        p v not p -> f ^ not f ==> False 
    Tested equiv with:
        p v not p <-> p v not p ==> True
        p v not p <-> f v not f ==> True
        p ^ not p <-> f ^ not f ==> True
        p v not p <-> f ^ not f ==> False
-}
-- Test cases.
tautP, tautQ, conjP, conjQ :: Form
tautP = Dsj [p, Neg p]
tautQ = Dsj [q, Neg q]
conjP = Cnj [p, Neg p]
conjQ = Cnj [q, Neg q]

assignment1 = do
    print "Assignment 1"
    print (tautology tautP)
    print (contradiction conjP)
    print (entails tautP tautP)
    print (entails tautP tautQ)
    print (entails conjP conjQ)
    print (entails tautP conjQ)
    print (equiv tautP tautP)
    print (equiv tautP tautQ)
    print (equiv conjP conjP)
    print (equiv tautP conjQ)

-- 2 - testing parse

parseTautP = "+(1 -1)"
parseTautQ = "+(2 -2)"
parseConjP = "*(1 -1)"
parseConjQ = "*(2 -2)"
testSingleDsj = parse "+(1)" !! 0
-- (q v q v q v q)
testMulitpleDsj = parse "+(2 2 2 2 2)" !! 0
--(q v r v r2 V r3 V r4)
-- Only 1 False output
testMulitpleDsj2 = parse "+(2 3 4 5 6)" !! 0
-- p
testSingleCnj = parse "*(1)" !! 0
--(q ^ r ^ r2 ^ r3 ^ r4)print (map (flip evl testMulitpleDsj) (allVals testMulitpleDsj))
-- Only 1 True output
testMulitpleCnj = parse "*(2 3 4 5 6)" !! 0
-- Parseing weard stuff still works
weirdParse = "+(1 1) asfsajfsdjklafndsa ()(#(#$&#@^&$(@#*()_)!_!__!@*!@_+!"
-- Parse has max number
parseLarge = "(1 ==> 999999999999999999999999)"

parseArrows = ["(1 ==> 2)", "+((1 ==> 2) 3 (3 <=> 3))"]

assignment2 = do
    print "Assignment 2"
    print (parse parseTautP !! 0 == tautP)
    print (parse parseTautQ !! 0 == tautQ)
    print (parse parseConjP !! 0 == conjP)
    print (parse parseConjQ !! 0 == conjQ)
    print (map (flip evl testSingleDsj) (allVals testSingleDsj))
    print (map (flip evl testMulitpleDsj) (allVals testMulitpleDsj))
    print (map (flip evl testMulitpleDsj2) (allVals testMulitpleDsj2))
    print (map (flip evl testSingleCnj) (allVals testSingleCnj))
    print (map (flip evl testMulitpleCnj) (allVals testMulitpleCnj))
    print "You can parse weard stuff and it will still work"
    print ("Parseing: " ++ weirdParse)
    print (parse weirdParse)
    print ("You have a max number to parse: " ++ parseLarge)
    print (parse parseLarge)
    print ([parse x !! 0 | x <- parseArrows])

main = do
    assignment1
    assignment2
    