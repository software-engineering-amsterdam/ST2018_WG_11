module Lab5 where

import Data.List
import System.Random
import Control.Monad
import System.CPUTime
import Lecture5_1 (assignment1)
import Lecture5_2 (assignment2,assignment4,assignment5)

example1 :: [[Int]]
example1 = [[5,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example2 :: [[Int]]
example2 = [[0,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example3 :: [[Int]]
example3 = [[1,0,0,0,3,0,5,0,4],
            [0,0,0,0,0,0,0,0,3],
            [0,0,2,0,0,5,0,9,8], 
            [0,0,9,0,0,0,0,3,0],
            [2,0,0,0,0,0,0,0,7],
            [8,0,3,0,9,1,0,6,0],
            [0,5,1,4,7,0,0,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,4,0,0,0,9,7,0,0]]

example4 :: [[Int]]
example4 = [[1,2,3,4,5,6,7,8,9],
            [2,0,0,0,0,0,0,0,0],
            [3,0,0,0,0,0,0,0,0],
            [4,0,0,0,0,0,0,0,0],
            [5,0,0,0,0,0,0,0,0],
            [6,0,0,0,0,0,0,0,0],
            [7,0,0,0,0,0,0,0,0],
            [8,0,0,0,0,0,0,0,0],
            [9,0,0,0,0,0,0,0,0]]

example5 :: [[Int]]
example5 = [[1,0,0,0,0,0,0,0,0],
            [0,2,0,0,0,0,0,0,0],
            [0,0,3,0,0,0,0,0,0],
            [0,0,0,4,0,0,0,0,0],
            [0,0,0,0,5,0,0,0,0],
            [0,0,0,0,0,6,0,0,0],
            [0,0,0,0,0,0,7,0,0],
            [0,0,0,0,0,0,0,8,0],
            [0,0,0,0,0,0,0,0,9]]

nrcExample :: [[Int]]
nrcExample = [[0,0,0,3,0,0,0,0,0],
              [0,0,0,7,0,0,3,0,0],
              [2,0,0,0,0,0,0,0,8],
              [0,0,6,0,0,5,0,0,0],
              [0,9,1,6,0,0,0,0,0],
              [3,0,0,0,7,1,2,0,0],
              [0,0,0,0,0,0,0,3,1],
              [0,8,0,0,4,0,0,0,0],
              [0,0,2,0,0,0,0,0,0]] 

nrcExample2 :: [[Int]]
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
-- 180 minutes

-- Look at Lecture5_2.hs for the refactored code.

-- The refactored code is way more easier to modify
-- If you want to add new constraints you need to make one
--  2d array of the cells that aren considered a set. and
--  just add this 2d array to the list of constraints.

-- TODO add QuickCheck?

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

-- If the problem is minimal this means that if you remove
--  one elemement the problem is ambigues. 


-- Exercise 4
-- 120 minutes

-- Exercise 5
-- 60 minutes
-- Generating this tends to take a really long time.
-- Expect somewhere between 50 and 100 seconds.
-- The reason for this is minimalizing the sudoku.
-- Because the constraints are tighter more values can be absent.
-- This is logical because the more constraints -> the easier it is to fill in values.
-- If more values are absent the tree runs way deeper.
-- And because the tree is way deeper it takes more time to compute all possibilities.
-- TODO: Add calculations to prove this.

main = do 
    putStrLn "Exercise 1"
    assignment1 nrcExample

    putStrLn "\nExercise 2"
    assignment2 nrcExample
    assignment2 nrcExample2
    putStrLn "\nTest timing of assignment 1 vs 2 a couple of times."
    putStrLn "Left is assignment 1 and right is assignment 2."
    putStrLn "Test NRC example"
    time (assignment1 nrcExample) (assignment2 nrcExample)
    time (assignment1 nrcExample2) (assignment2 nrcExample2)
    putStrLn "\nTest example 1 through 5"
    time (assignment1 example1) (assignment2 example1)
    time (assignment1 example2) (assignment2 example2)
    time (assignment1 example3) (assignment2 example3)
    time (assignment1 example4) (assignment2 example3)
    time (assignment1 example5) (assignment2 example4)

    -- putStrLn "\nExercise 4"
    -- assignment4

    -- putStrLn "\nExercise 5"
    -- assignment5


    