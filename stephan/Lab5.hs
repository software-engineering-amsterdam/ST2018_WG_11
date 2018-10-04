
module Lab5
where 
import Data.List
import System.Random
import Test.QuickCheck
import Lab5_ex1 (assingment1)
import Lab5_ex2 (rsolveNsEx_2)
import Lecture5
import Text.Printf
import Control.Exception
import System.CPUTime

-- import Lab5_ex2

-- 1 See Lab5_ex1.hs Time: 5 hours

-- 2 See Lab5_ex2.hs Time: 4 hours
{-
    The new way is much easier to change to NRC then before;
        You only have to add one constraints list to allConstrnts line: 79 and you are done
        In the previous way you had to change it in a lot of places and implement it in a lot of diffrent ways.

    The old method is quicker
        See results below
-}
-- https://wiki.haskell.org/Timing_computations

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

assingment2 = do
    print "Time example 1 original"
    time (rsolveNs $ initNode example1)
    print "Time example 1 New"
    time (rsolveNsEx_2 $ initNode example1)
    print "Time example 2 original"
    time (rsolveNs $ initNode example2)
    print "Time example 2 New"
    time (rsolveNsEx_2 $ initNode example2)
    print "Time example 3 original"
    time (rsolveNs $ initNode example3)
    print "Time example 3 New"
    time (rsolveNsEx_2 $ initNode example3)
    print "Time example 4 original"
    time (rsolveNs $ initNode example4)
    print "Time example 4 New"
    time (rsolveNsEx_2 $ initNode example4)
    print "Time example 5 original"
    time (rsolveNs $ initNode example5)
    print "Time example 5 New"
    time (rsolveNsEx_2 $ initNode example5)

-- 3 1.5 hours

{-
    A Sudoku problem P is minimal if it admits a unique 
    solution, and every problem P' you can get from P 
    by erasing one of the hints admits more than one 
    solution. How can you test whether the problems 
    generated by the code given in the lecture notes 
    are minimal?
-}

{-
    Generates random Node
    First checks if a unique solution
    Second checks if you remove one element if it is not an 
        unique solution, for all elements in sudoku
-}
testMinimalProblem :: Int -> IO Bool
testMinimalProblem count = do
    [random] <- rsolveNs [emptyN]
    minimalSud <- genProblem random
    trueTest <- return $ uniqueSol minimalSud
    falseTest <- return $ all (==False) [uniqueSol n | n <- removeOne minimalSud]
    nextTest <- if count < 1 then 
        return True else
            testMinimalProblem (count - 1)
    print $ "Test " ++ (show count) ++ ": " ++ (show (trueTest && falseTest))
    return $ (trueTest && nextTest && falseTest)

-- Remove one item from node for every item in node
removeOne :: Node -> [Node]
removeOne (s, _ ) = nodeMinusOneElement s (filledPositions s)

nodeMinusOneElement :: Sudoku -> [(Row,Column)] -> [Node]
nodeMinusOneElement s [] = []
nodeMinusOneElement s ((r,c):xs) = eraseN (s, []) (r,c) : 
    (nodeMinusOneElement s xs)

assingment3 = do
    print "Assingment 3"
    testMinimalProblem 10

-- 4

removeBlock :: Node -> (Row, Column) -> Node
removeBlock n (r, c) = foldl (eraseN) n [(r',c') | r' <- bl r, c' <- bl c ]

-- test = genEmptyBlock (removeBlock (head $ initNode example2) (1, 1))
test = (head $ initNode example2)

removeRandomBlock :: Node -> IO Node
removeRandomBlock n = do ys <- randomize [(x,y) | x <- [1,4,7], y <- [1,4,7]]
                         return (removeBlock n (head ys))                            
                         
genMinimize :: Node -> IO Node
genMinimize n = do ys <- randomize xs
                   return (minimalize n ys)
                        where xs = filledPositions (fst n)
testtest :: Node -> IO Node
testtest node = do 
            n <- removeRandomBlock node
            m <- genMinimize n
            sol <- if uniqueSol m then return m else testtest node
            return $ sol
            
main2 = do
    print "Lab 5"
    assingment1
    assingment2
    assingment3