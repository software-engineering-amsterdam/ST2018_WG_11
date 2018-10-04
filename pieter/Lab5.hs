module Lab5 where

import Data.List
import System.Random
import System.CPUTime
import Text.Printf
import Lecture5_1 (assignment1)
import Lecture5_2 (assignment2)


-- Exercise 1 
-- 120 minutes
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

-- Function for comparing the runtime of functions
time :: IO t -> IO t -> IO Double
time a b = do
    start1 <- getCPUTime
    v1 <- a `seq` return ()
    end1   <- getCPUTime
    start2 <- getCPUTime
    v2 <- b `seq` return ()
    end2   <- getCPUTime
    let diff1 = (fromIntegral (end1 - start1)) / (10^12)
    let diff2 = (fromIntegral (end2 - start2)) / (10^12)
    let diff = diff1 - diff2
    putStrLn ""
    putStrLn ("Time difference " ++ (show diff))
    putStrLn ("   Left time " ++ (show diff1))
    putStrLn ("   Right time " ++ (show diff2))
    return diff

main = do 
    putStrLn "Exercise 1"
    assignment1
    putStrLn "Exercise 2"
    assignment2
    putStrLn "Test timing of assignment 1 vs 2 a couple of times."
    time assignment1 assignment2
    time assignment1 assignment2
    time assignment1 assignment2
    time assignment1 assignment2
    time assignment1 assignment2


    