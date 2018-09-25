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

-- (p ^ (q v r) ^ r2)
testCnjDsj = parse "*(1 +(2 3) 4)" !! 0

parseArrows = ["(1 ==> 2)", "+((1 ==> 2) 3 (3 <=> 3))"]

-- Automated tests

{-
    When you create a Form from a string, show it and parse it again
    it should give the same result.

    Should always return True or give unkown token error.
-}
testParseString :: String -> Bool
testParseString x = (length (parse x) > 0) --> (parse x)
                        == (parse (show (head (parse x))))

{-
    Should always return True
-}
testParseForm :: Form -> Bool
testParseForm x = head (parse (show x)) == x

assignment2 = do
    print "Assignment 2 - manual testing"
    print (parse parseTautP !! 0 == tautP)
    print (parse parseTautQ !! 0 == tautQ)
    print (parse parseConjP !! 0 == conjP)
    print (parse parseConjQ !! 0 == conjQ)
    print (map (flip evl testSingleDsj) (allVals testSingleDsj))
    print (map (flip evl testMulitpleDsj) (allVals testMulitpleDsj))
    print (map (flip evl testMulitpleDsj2) (allVals testMulitpleDsj2))
    print (map (flip evl testSingleCnj) (allVals testSingleCnj))
    print (map (flip evl testMulitpleCnj) (allVals testMulitpleCnj))
    print "You can parse weird stuff and it will still work"
    print ("Parseing: " ++ weirdParse)
    print (parse weirdParse)
    print ("You have a max number to parse: " ++ parseLarge)
    print (parse parseLarge)
    print ([parse x !! 0 | x <- parseArrows])
    print (allVals testCnjDsj)
    print "assingment 2 - functionable testing"
    print (all (\x -> testParseString x) [parseTautP,parseTautQ,parseConjP,parseConjQ])
    print (all (\x -> testParseForm x) [tautP,tautQ, conjP,conjQ])


-- 3 time: 7 hours
{-
    The lecture notes of this week discuss the conversion of Boolean
    formulas (formulas of propositional logic) into CNF form. The lecture
    notes also give a definition of a Haskell datatype for formulas of
    propositional logic, using lists for conjunctions and disjunctions.
    Your task is to write a Haskell program for converting formulas into CNF.
-}

test,test2 :: Form
test = Dsj [Cnj [p,q], r]
test2 = Dsj [r, Cnj [p,q]]
test3 = Dsj [r,p ,q,Cnj[q,r,p]]

-- Forces pairs on cnf and dnf so (p1^p2^p3) will become (p1^(p2^p3))
-- This makes the math much easier for the next functions
toPair :: Form -> Form
toPair (Prop x) = Prop x
toPair (Neg x) = (Neg x)
toPair (Cnj [x]) = toPair x
toPair (Cnj [x,y]) = Cnj [toPair x,toPair y]
toPair (Cnj (x:y:zs)) = Cnj [toPair x, toPair (Cnj (y:zs))]
toPair (Dsj [x]) = toPair x
toPair (Dsj [x,y]) = Dsj [toPair x,toPair y]
toPair (Dsj (x:y:zs)) = Dsj [toPair x, toPair (Dsj (y:zs))]

--Distribute ORs inwards over ANDs: repeatedly replace P ∨ ( Q ∧ R )
-- ( P ∨ Q ) ∧ ( P ∨ R )
-- Since there is a max of 2 items in the array there are just 3 possiblity
-- 1) Cnj on first place, 2) on second, 3) not present

check :: Form -> Bool
check (Prop x) = False
check (Neg x) = False
check (Cnj x) = (any check x)
check (Dsj x) = (any check2 x)

check2 :: Form -> Bool
check2 (Prop x) = False
check2 (Neg x) = False
check2 (Cnj _) = True
check2 (Dsj x) = (any check2 x)

cnf1 :: Form -> Form
cnf1 (Prop x) = Prop x
cnf1 (Neg (Prop x)) = Neg (Prop x)
cnf1 (Cnj xs) = Cnj (map cnf1 xs)
cnf1 (Dsj [Cnj [x,y], z]) = if (check x || check y || check z) then
                                cnf1 $! (rp) else rp
                            where
                                rp = Cnj [(cnf1 (Dsj [x,z])), (cnf1 (Dsj [y,z]))]
cnf1 (Dsj [z, Cnj [x,y]]) = if (check x || check y || check z) then
                                cnf1 $! (rp) else rp
                            where
                                rp = Cnj [(cnf1 (Dsj [x,z])), (cnf1 (Dsj [y,z]))]
cnf1 (Dsj [x, y]) = let rp = (Dsj (map cnf1 [x, y])) in if (check rp) then cnf1 $! rp else rp

cnf :: Form -> Form
cnf form = cnf1 formx
            where formx = toPair (nnf (arrowfree form))

-- 4 time: 8 hours

{-
    Write a formula generator for random testing of properties of
    propositional logic, or teach yourself enough QuickCheck to use
    random QuickCheck testing of formulas.

    Use your random testing method to test the correctness of the
    conversion program from the previous exercise. Formulate a number
    of relevant properties to test, and carry out the tests, either with
    your own random formula generator or with QuickCheck.

    Deliverables: generator for formulas, sequence of test properties,
    test report, indication of time spent.
-}

{-
data Form = Prop Name
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form
          | Equiv Form Form
          deriving (Eq,Ord)
-}

randomOp :: IO String
randomOp = do
    x <- randomRIO(0,50) :: IO Int
    return $ if x <= 10 then "-"
                else if x <= 20 then "+"
                else if x <= 30 then "*"
                else if x <= 40 then "==>"
                else "<=>"

randomProp :: IO String
randomProp = do
    x <- randomRIO(1,100) :: IO Int
    return $ show x

randomForm :: Int -> IO String
randomForm layer = do
    op <- randomOp
    randomFactor1 <- randomRIO (0,100) :: IO Int
    randomFactor2 <- randomRIO (0,100) :: IO Int
    prop <- if (layer > 0 && randomFactor1 > 50) then randomForm (layer - 1) else randomProp
    prop2 <- if (layer > 0 && randomFactor2 > 50) then randomForm (layer - 1) else randomProp
    return $ if op == "-" then op ++ prop
                else if (op == "==>" || op == "<=>")
                    then
                        if (False)
                            then "(" ++ prop ++ " " ++ op ++ " " ++ prop2 ++ ")"
                                else "(" ++ prop ++ " " ++ op ++ " " ++ prop2 ++ ")"
                    else op ++ "(" ++ prop ++ " " ++ prop2 ++ ")"

randomForms :: IO Form
randomForms = do
                length <- randomRIO (5,15)
                x <- randomForm length
                return $ head (parse x)

main = do
    assignment1
    assignment2

-- (a -> b) -> [a] -> [b]
