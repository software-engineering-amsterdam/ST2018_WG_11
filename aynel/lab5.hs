module Lab5 where

import Data.List
import System.Random
import Lecture5

-- 1. Modified Sudoku solver. Time: 2.5 hours

{-

The goal of this exercise is to extend the Sudoku program described in the lecture of this week 
with functions that can also handle Sudokus of a special kind: the Sudokus that appear in the 
Dutch evening newspaper NRC-Handelsblad each week (designed by Peter Ritmeester, from Oct 8, 2005 onward). 
These NRC Sudokus are special in that they have to satisfy a few extra constraints: in addition to 
the usual Sudoku constraints, each of the 3×3 subgrids with left-top corner (2,2), (2,6), (6,2),
and (6,6) should also yield an injective function. The above figure gives an example (this is the NRC 
sudoku that appeared Saturday Nov 26, 2005).

Your task is to formalize this extra constraint, and to use your formalization in a program that can 
solve this Sudoku. See also the webpage of Andries Brouwer.

Deliverables: modified Sudoku solver, solution to the above puzzle, indication of time spent.

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

-- modified sudoku solver is in Lecture5.hs

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
    solveAndShow exampleNRC


-- 2. Refactor the code along the lines of this proposal, and next compare the two versions for extendability and efficiency. 

assignment2 = do
    print "Assignment2"