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
tautP, tautQ, conjP, conjQ :: Form
tautP = Dsj [p, Neg p]
tautQ = Dsj [q, Neg q]
conjP = Cnj [p, Neg p]
conjQ = Cnj [q, Neg q]

parseTautP = "+(1 -1)"
parseTautQ = "+(2 -2)"
parseConjP = "*(1 -1)"
parseConjQ = "*(2 -2)"

testSingleDsj = parse "+(1)" !! 0 -- Does this work?
testMulitpleDsj = parse "+(2 2 2 2 2)" !! 0 -- Only 2 evl possible
testMulitpleDsj2 = parse "+(2 3 4 5 6)" !! 0 -- When evl 1 False rest True

testSingleCnj = parse "*(1)" !! 0 -- Does this work?
testMulitpleCnj = parse "*(2 3 4 5 6)" !! 0 -- When evl 1 True rest False

weirdParse = "+(1 1) asfsajfsdjklafndsa ()(#(#$&#@^&$(@#*()_)!_!__!@*!@_+!"
parseLarge = "(1 ==> 999999999999999999999999)"

-- -- (p ^ (q v r) ^ r2)
-- testCnjDsj = parse "*(1 +(2 3) 4)" !! 0
-- parseArrows = ["(1 ==> 2)", "+((1 ==> 2) 3 (3 <=> 3))"]


-- propertie testing
{-
    When you create a Form from a string, show it and parse it again
    it should give the same result.

    Should always return True or give unkown token error.
-}
testParseString :: String -> Bool
testParseString x = (length (parse x) > 0) --> (parse x)
                        == (parse (show (head (parse x))))



assignment2 = do
    print "exercise 2"
    print (parse parseTautP !! 0 == tautP)
    print (parse parseTautQ !! 0 == tautQ)
    print (parse parseConjP !! 0 == conjP)
    print (parse parseConjQ !! 0 == conjQ)
    print "You can parse weird stuff and it will still work"
    print ("Parseing: " ++ weirdParse ++ ". will give:")
    print (parse weirdParse)
    print ("You have a max number to parse: " ++ parseLarge)
    print (parse parseLarge)
    print (map (flip evl testSingleDsj) (allVals testSingleDsj))
    print (map (flip evl testMulitpleDsj) (allVals testMulitpleDsj))
    print (map (flip evl testMulitpleDsj2) (allVals testMulitpleDsj2))
    print (map (flip evl testSingleCnj) (allVals testSingleCnj))
    print (map (flip evl testMulitpleCnj) (allVals testMulitpleCnj))
    print (all (\x -> testParseString x) [parseTautP,parseTautQ,parseConjP,parseConjQ])

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

-- tests
td1 = Dsj [p,Cnj [p,q,r],Dsj [p]]
td2 = Cnj [p, Dsj [p,q,r], Cnj [p]]

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

-- automated test
autoTesting :: Int -> IO Bool
autoTesting count = do
                form <- randomForm
                nextTest <- if count > 0 then autoTesting (count - 1) else return True
                return $ isCnf (cnf form) && nextTest

assignment3 = do
    print "exercise 3"
    print "Check equivalence with original"
    print (equiv (cnf form1) form1)
    print (equiv (cnf form2) form2)
    print (equiv (cnf form3) form3)
    print (equiv (cnf td1) td1)
    print (equiv (cnf td2) td2)
    print "Reapplying cnf on an already cnf should do nothing"
    print (cnf form1 == (cnf (cnf form1)))
    print (cnf form2 == (cnf (cnf form2)))
    print (cnf form3 == (cnf (cnf form3)))
    print "Test with longer cnj and dsj arrays"
    print (cnf (parse "+(2 3 4 5 *(6 7 8 9))" !! 0))
    print (cnf (parse "*(2 3 4 5 +(6 7 8 9))" !! 0))
    autoTesting 10

-- 4. Generator.


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

randomFormString :: Int -> IO String
randomFormString layer = do
    op <- randomOp
    randomFactor1 <- randomRIO (0,100) :: IO Int
    randomFactor2 <- randomRIO (0,100) :: IO Int
    prop <- if (layer > 0 && randomFactor1 > 50) then randomFormString (layer - 1) else randomProp
    prop2 <- if (layer > 0 && randomFactor2 > 50) then randomFormString (layer - 1) else randomProp
    return $ if op == "-" then op ++ prop
                else if (op == "==>" || op == "<=>")
                    then
                        if (False)
                            then "(" ++ prop ++ " " ++ op ++ " " ++ prop2 ++ ")"
                                else "(" ++ prop ++ " " ++ op ++ " " ++ prop2 ++ ")"
                    else op ++ "(" ++ prop ++ " " ++ prop2 ++ ")"

randomForm :: IO Form
randomForm = do
                length <- randomRIO (5,15)
                x <- randomFormString length
                return $ head (parse x)

assignment4 = do
              print "exercise 4"
              f1 <- randomForm
              f2 <- randomForm
              f3 <- randomForm
              f4 <- randomForm
              
              print "Show some randomly generated forms"
              print f1
              print f2
              print f3
              print f4
              print "Apply cnf for testing purposes"
              print (cnf f1)
              print (cnf f2)
              print (cnf f3)
              print (cnf f4)

-- 5. Bonus.


main = do
    assignment1
    assignment2
    assignment3
    assignment4