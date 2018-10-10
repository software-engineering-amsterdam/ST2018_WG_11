module Lab5 where

import Data.List
import System.Random
import Control.Monad
import System.CPUTime
import Lecture5_1 (assignment1,rsolveNsEx_1)
import Lecture5_2 (assignment2,assignment5,rsolveNsEx_2)
import Lecture5

nrcExample :: Grid
nrcExample = [[0,0,0,3,0,0,0,0,0],
              [0,0,0,7,0,0,3,0,0],
              [2,0,0,0,0,0,0,0,8],
              [0,0,6,0,0,5,0,0,0],
              [0,9,1,6,0,0,0,0,0],
              [3,0,0,0,7,1,2,0,0],
              [0,0,0,0,0,0,0,3,1],
              [0,8,0,0,4,0,0,0,0],
              [0,0,2,0,0,0,0,0,0]] 

nrcExample2 :: Grid
nrcExample2 = [[0,3,0,0,0,0,0,0,0],
               [0,9,8,0,0,0,0,0,0],
               [0,4,0,0,2,0,0,1,0],
               [0,0,1,0,0,0,9,0,0],
               [0,0,0,0,8,0,0,3,0],
               [3,0,0,0,4,7,0,0,0],
               [0,0,6,0,0,0,3,0,0],
               [0,0,0,0,0,0,6,0,1],
               [0,0,0,0,0,0,0,0,0]] 

-- Exercise 1 
-- median 3,5 hours
-- Look at Lecture5_1.hs for the updated code.
{--
Solution
    +-------+-------+-------+
    | 4 7 8 | 3 9 2 | 6 1 5 |
    | 6 1 9 | 7 5 8 | 3 2 4 |
    | 2 3 5 | 4 1 6 | 9 7 8 |
    +-------+-------+-------+
    | 7 2 6 | 8 3 5 | 1 4 9 |
    | 8 9 1 | 6 2 4 | 7 5 3 |
    | 3 5 4 | 9 7 1 | 2 8 6 |
    +-------+-------+-------+
    | 5 6 7 | 2 8 9 | 4 3 1 |
    | 9 8 3 | 1 4 7 | 5 6 2 |
    | 1 4 2 | 5 6 3 | 8 9 7 |
    +-------+-------+-------+
--}


-- Exercise 2
-- 3,5 hours

-- Look at Lecture5_2.hs for the refactored code.

-- The refactored code is way more easier to modify
-- If you want to add new constraints you need to make one
--  2d array of the cells that aren considered a set. and
--  just add this 2d array to the list of constraints.

-- Function for comparing the runtime of functions
-- https://wiki.haskell.org/Timing_computations
time a b = do
    -- Run the test a lot of times to get a better average
    let nTimes = 1000000
    -- Start running tests
    start1 <- getCPUTime
    v1 <- replicateM nTimes (a `seq` return ())
    end1   <- getCPUTime
    start2 <- getCPUTime
    v2 <- replicateM nTimes (b `seq` return ())
    end2   <- getCPUTime
    -- Calculate difference between start and end time
    let diff1 = (fromIntegral (end1 - start1)) / (10^12)
    let diff2 = (fromIntegral (end2 - start2)) / (10^12)
    -- Difference between two tests
    let diff = diff1 - diff2
    -- putStrLn ("Ran test " ++ (show nTimes) ++ " times")
    putStrLn ("Time difference " ++ (show diff))
    putStrLn ("   Left time " ++ (show diff1))
    putStrLn ("   Right time " ++ (show diff2))

-- Exercise 3
-- 180 minuts
{-
    Generates random Node
    First checks if a node has just one solution
    Second checks if you remove one element if it has more then one solution.
        (Checks all elements)
-}
testMinimalProblem :: Int -> IO Bool
testMinimalProblem count = do
    [random] <- rsolveNs [emptyN]
    minimalSud <- genProblem random
    oneSolution <- return $ uniqueSol minimalSud
    ifRemoveMoreSol <- return $ all (==False) 
                        [uniqueSol n | n <- removeOne minimalSud]
    nextTest <- if count < 1 then 
        return True else
            testMinimalProblem (count - 1)
    print $ "Test " ++ (show count) ++ ": " ++ 
            (show (oneSolution && ifRemoveMoreSol))
    return $ (oneSolution && ifRemoveMoreSol && nextTest)

-- Returns all nodes were one unique element has been removed
removeOne :: Node -> [Node]
removeOne (s, _ ) = nodeMinusOneElement s (filledPositions s)

-- Returns all nodes were one unique element has been removed
nodeMinusOneElement :: Sudoku -> [(Row,Column)] -> [Node]
nodeMinusOneElement s [] = []
nodeMinusOneElement s ((r,c):xs) = eraseN (s, []) (r,c) : 
    (nodeMinusOneElement s xs)

assignment3 = do
    -- Takes long so limited to 2
    testMinimalProblem 2

-- Exercise 4
-- 180 minutes

blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]

safeHead :: [a] -> [a]
safeHead [] = []
safeHead (x:xs) = [x]

-- Remove  a block of the 3x3 grid.
removeBlock :: Node -> (Row, Column) -> Node
removeBlock n (r, c) = foldl (eraseN) n (concat [x | x <- blockConstrnt, (r,c) `elem` x])

removeBlocks :: Node -> [(Row,Column)] -> Node
removeBlocks node xs = foldl (\p q -> removeBlock p q) node xs

minimize :: Node -> Node
minimize nod = minimalize nod (filledPositions (fst nod))

-- Try to find a solution where we can remove n blocks and keep uniqueness.
removeAndMinimalize node n = do
    -- Get all blocks in the sudoku grid.
    let blocks = [(x,y) | x <- [1,4,7], y <- [1,4,7]]
    -- Get all combinations the sudoku can make
    let combinations = permutations blocks
    -- Get all possible 3 combinations of 3 blocks
    let possibilities = nub [take n x | x <- combinations]
    -- Check multiple combinations in the 3x3 grid
    -- Return the first one that has a unique solution
    let solvable = safeHead [x | x <- possibilities, uniqueSol(removeBlocks node x)]
    if ((length solvable) == 0) then
        putStrLn $ "Could not find a solution with " ++ (show n) ++ " blocks removed."
    else
        -- let solution = removeBlocks node (head solvable)
        showNode (minimize (removeBlocks node (head solvable)))

-- Remove n random block from the 3x3 grid.
removeRandomBlocks :: Node -> Int -> IO Node
removeRandomBlocks node n = do 
        options <- randomize [(x,y) | x <- [1,4,7], y <- [1,4,7]]
        let choices = take n options      
        return (removeBlocks node choices)-- (foldl (\p q -> removeBlock p q) node choices)  

-- Function for removing n block and minimalizing the sudoku.
-- This does not always work because of ambiguity. Think of removing top 3 blocks.
randomRemoveAndMinimalize orig n = do
    new <- removeRandomBlocks orig n
    showNode new
    let min = minimalize new (filledPositions (fst new))
    showNode min
  
  
assignment4 = do
    -- Whole sudoku
    [original] <- rsolveNs [emptyN]
    showNode original
    putStrLn "Randomly removing three does not always work."
    randomRemoveAndMinimalize original 3
    putStrLn "Try to remove 3 blocks from the original and get an unique minimal solution."
    -- Always successful
    removeAndMinimalize original 3
    putStrLn "Try to remove 4 blocks from the original and get an unique minimal solution."
    putStrLn "WARNING: can take a while."
    -- Not always successful
    removeAndMinimalize original 4
    putStrLn "Not been able to find any by removing 5 blocks at the moment."
    -- Uncomment if you want to try but have not found any yet
    -- putStrLn "Try to remove 5 blocks from the original and get an unique minimal solution."
    -- removeAndMinimalize original 5

-- Exercise 5
-- 60 minutes
-- Generating this tends to take a really long time.
-- Expect somewhere between 50 and 100 seconds.
-- The reason for this is minimalizing the sudoku.
-- Because the constraints are tighter more values can be absent.
-- This is logical because the more constraints -> the easier it is to fill in values.
-- If more values are absent the tree runs way deeper.
-- And because the tree is way deeper it takes more time to compute all possibilities.

main = do 
    putStrLn "Exercise 1"
    assignment1 nrcExample

    putStrLn "\nExercise 2"
    putStrLn "Solve two known NRC sudoku's"
    assignment2 nrcExample
    assignment2 nrcExample2
    putStrLn "\nLeft is assignment 1 and right is assignment 2."
    putStrLn "Test timing assignment 1 and 2 on random solve."
    time (rsolveNsEx_1) (rsolveNsEx_2)
    time (rsolveNsEx_1) (rsolveNsEx_2)
    putStrLn "\nTest timing of assignment 1 vs 2 a couple of times."
    putStrLn "Test NRC example"
    time (assignment1 nrcExample) (assignment2 nrcExample)
    time (assignment1 nrcExample2) (assignment2 nrcExample2)
    putStrLn "\nTest example 1 through 5"
    time (assignment1 example1) (assignment2 example1)
    time (assignment1 example2) (assignment2 example2)
    time (assignment1 example3) (assignment2 example3)
    time (assignment1 example4) (assignment2 example3)
    time (assignment1 example5) (assignment2 example4)

    putStrLn "\nExercise 3"
    assignment3
    putStrLn "\nExercise 4"
    assignment4

    putStrLn "\nExercise 5"
    assignment5


    