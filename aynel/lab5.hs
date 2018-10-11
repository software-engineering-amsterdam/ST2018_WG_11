module Lab5 where

import Data.List
import System.Random
-- import Lecture5
import Lecture5_2

-- 1. Modified Sudoku solver. Time: 2.5 hours

{-

+---------+---------+---------+
|         | 3       |         |
|   +-----|--+   +--|-----+   |
|   |     | 7|   |  | 3   |   |
| 2 |     |  |   |  |     | 8 |
+---------+---------+---------+
|   |   6 |  |   |5 |     |   |
|   +-----|--+   +--|-----+   |
|    9  1 | 6       |         |
|   +-----|--+   +--|-----+   |
| 3 |     |  | 7 |1 | 2   |   |
+---------+---------+---------+
|   |     |  |   |  |    3| 1 |
|   |8    |  | 4 |  |     |   |
|   +-----|--+   +--|-----+   |
|       2 |         |         |
+---------+---------+---------+

-}

-- modified sudoku solver is in Lecture5.hs. Time: 3 hours

exampleNRC :: Grid
exampleNRC = [[0,0,0,3,0,0,0,0,0],
              [0,0,0,7,0,0,3,0,0],
              [2,0,0,0,0,0,0,0,8],
              [0,0,6,0,0,5,0,0,0],
              [0,9,1,6,0,0,0,0,0],
              [3,0,0,0,7,1,2,0,0],
              [0,0,0,0,0,0,0,3,1],
              [0,8,0,0,4,0,0,0,0],
              [0,0,2,0,0,0,0,0,0]]

assignment1 = do
    print "Assignment1"
    print "solution example NRC Sudoku: "
    -- solveAndShow exampleNRC


-- 2. Refactor the code along the lines of this proposal, and next compare the two versions for extendability and efficiency. Time: 3.5 hours

assignment2 = do
    print "Assignment2"
    -- freeAtPos' (grid2sud exampleNRC) (2,4) blockConstrnt

    -- this version is slower, but is easer to modify for NRC sudoku's


-- 3. Minimal Sudoku problem. Time: ... 
-- removeHint:: Node -> [Node]
-- removeHint