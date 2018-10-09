module Lecture5_2

where 

import Data.List
import System.Random
import Control.Monad
import System.CPUTime

type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]

type Position = (Row,Column)
type Constrnt = [[Position]]

positions, values :: [Int]
positions = [1..9]
values    = [1..9] 

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

showVal :: Value -> String
showVal 0 = " "
showVal d = show d

showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+-------+-------+-------+")
    showRow as; showRow bs; showRow cs
    putStrLn ("+-------+-------+-------+")
    showRow ds; showRow es; showRow fs
    putStrLn ("+-------+-------+-------+")
    showRow gs; showRow hs; showRow is
    putStrLn ("+-------+-------+-------+")

type Sudoku = (Row,Column) -> Value

sud2grid :: Sudoku -> Grid
sud2grid s = 
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c) 
  where 
  pos :: [[a]] -> (Row,Column) -> a 
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

-- Updated to check all constraints
consistent :: Sudoku -> Bool
consistent s = all (==True) [injective [s c | c <- con, (s c) /=0] | con <- constraintsList]

extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
extend = update

update :: Eq a => (a -> b) -> (a,b) -> a -> b 
update f (y,z) x = if x == y then z else f x 

type Constraint = (Row,Column,[Value])

type Node = (Sudoku,[Constraint])

showNode :: Node -> IO()
showNode = showSudoku . fst

solved  :: Node -> Bool
solved = null . snd

extendNode :: Node -> Constraint -> [Node]
extendNode (s,constraints) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         prune (r,c,v) constraints) | v <- vs ]

-- Updated to check for all the constraints
prune :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
prune _ [] = []
prune (r,c,v) ((x,y,zs):rest) 
      | sameSet (r,c) (x,y) = (x,y,zs\\[v]) : prune (r,c,v) rest
      | otherwise = (x,y,zs) : prune (r,c,v) rest

-- Check if the the position conflicts with any of the constraints
sameSet :: (Row,Column) -> (Row,Column) -> Bool
sameSet (r,c) (x,y) = any (\list -> (r,c) `elem` list) matched
    where matched = [con | con <- constraintsList, (x,y) `elem` con ]

initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in 
              if (not . consistent) s then [] 
              else [(s, constraints s)]

openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,  
                            c <- positions, 
                            s (r,c) == 0 ]

length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

constraints :: Sudoku -> [Constraint] 
constraints s = sortBy length3rd 
    [(r,c, freeAtPos s (r,c)) | 
                       (r,c) <- openPositions s ]

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

exmple1 = T 1 [T 2 [], T 3 []]
exmple2 = T 0 [exmple1,exmple1,exmple1]

grow :: (node -> [node]) -> node -> Tree node 

grow step seed = T seed (map (grow step) (step seed))

count :: Tree a -> Int 
count (T _ ts) = 1 + sum (map count ts)

takeT :: Int -> Tree a -> Tree a
takeT 0 (T x _) = T x []
takeT n (T x ts) = T x $ map (takeT (n-1)) ts

search :: (node -> [node]) 
       -> (node -> Bool) -> [node] -> [node]
search children goal [] = []
search children goal (x:xs) 
  | goal x    = x : search children goal xs
  | otherwise = search children goal ((children x) ++ xs)

solveNs :: [Node] -> [Node]
solveNs = search succNode solved 

succNode :: Node -> [Node]
succNode (s,[]) = []
succNode (s,p:ps) = extendNode (s,ps) p 

solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr)

solveShowNs :: [Node] -> IO[()]
solveShowNs = sequence . fmap showNode . solveNs

emptyN :: Node
emptyN = (\ _ -> 0,constraints (\ _ -> 0))

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do n <- getRandomInt maxi
                      return [xs !! n]
                   where maxi = length xs - 1

randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs 
                  if null y 
                    then return []
                    else do ys <- randomize (xs\\y)
                            return (head y:ys)

sameLen :: Constraint -> Constraint -> Bool
sameLen (_,_,xs) (_,_,ys) = length xs == length ys

getRandomCnstr :: [Constraint] -> IO [Constraint]
getRandomCnstr cs = getRandomItem (f cs) 
  where f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

rsuccNode :: Node -> IO [Node]
rsuccNode (s,cs) = do xs <- getRandomCnstr cs
                      if null xs 
                        then return []
                        else return 
                          (extendNode (s,cs\\xs) (head xs))

rsolveNs :: [Node] -> IO [Node]
rsolveNs ns = rsearch rsuccNode solved (return ns)

rsearch :: (node -> IO [node]) 
            -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes = 
  do xs <- ionodes 
     if null xs 
       then return []
       else 
         if goal (head xs) 
           then return [head xs]
           else do ys <- rsearch succ goal (succ (head xs))
                   if (not . null) ys 
                      then return [head ys]
                      else if null (tail xs) then return []
                           else 
                             rsearch 
                               succ goal (return $ tail xs)

genRandomSudoku :: IO Node
genRandomSudoku = do [r] <- rsolveNs [emptyN]
                     return r

randomS = genRandomSudoku >>= showNode

uniqueSol :: Node -> Bool
uniqueSol node = singleton (solveNs [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

eraseS :: Sudoku -> (Row,Column) -> Sudoku
eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                     | otherwise      = s (x,y)

eraseN :: Node -> (Row,Column) -> Node
eraseN n (r,c) = (s, constraints s) 
  where s = eraseS (fst n) (r,c) 

minimalize :: Node -> [(Row,Column)] -> Node
minimalize n [] = n
minimalize n ((r,c):rcs) | uniqueSol n' = minimalize n' rcs
                         | otherwise    = minimalize n  rcs
  where n' = eraseN n (r,c)

filledPositions :: Sudoku -> [(Row,Column)]
filledPositions s = [ (r,c) | r <- positions,  
                              c <- positions, s (r,c) /= 0 ]

genProblem :: Node -> IO Node
genProblem n = do ys <- randomize xs
                  return (minimalize n ys)
   where xs = filledPositions (fst n)

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let 
  ys = filter (elem (r,c)) xs in 
  foldl1 intersect (map ((values \\) . map s) ys)

freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) = freeAtPos' s (r,c) (constraintsList)

-- Define all constrains
rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
nrcBlockConstrnt = [[(r,c) | r <- b1, c <- b2] | 
                      b1 <- [[2..4],[6..8]], 
                      b2 <- [[2..4],[6..8]]]

-- By putting them in one list it should be easy for a user to add constraints
constraintsList :: Constrnt
constraintsList = concat [rowConstrnt,columnConstrnt,blockConstrnt,nrcBlockConstrnt]
-- If you want to test without NRC compliency comment above function and use below
-- constraintsList = concat [rowConstrnt,columnConstrnt,blockConstrnt]

-- Remove  a block of the 3x3 grid.
removeBlock :: Node -> (Row, Column) -> Node
removeBlock n (r, c) = foldl (eraseN) n (concat [x | x <- blockConstrnt, (r,c) `elem` x])

-- Remove n random block from the 3x3 grid.
removeRandomBlocks :: Node -> Int -> IO Node
removeRandomBlocks node n = do 
        options <- randomize [(x,y) | x <- [1,4,7], y <- [1,4,7]]
        let choices = take n options      
        return (foldl (\p q -> removeBlock p q) node choices)                      

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

-- Execute a timing test for n times for a function.
timingTest a = do
    let nTimes = 1000000
    start <- getCPUTime
    v <- replicateM nTimes (a `seq` return ())
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    putStrLn ("Ran test " ++ (show nTimes) ++ " times")
    putStrLn ("Time difference " ++ (show diff))

assignment2 grid = do
  solveAndShow $ grid

-- Function for removing n block and minimalizing the sudoku.
-- User for assignment 4
removeAndMinimalize orig n = do
  new <- removeRandomBlocks orig n
  showNode new
  let min = minimalize new (filledPositions (fst new))
  showNode min


assignment4 = do
  -- Whole sudoku
  [original] <- rsolveNs [emptyN]
  showNode original

  putStrLn "Remove 3 blocks from the original and try to minimalize."
  removeAndMinimalize original 3
  putStrLn "Remove 4 blocks from the original and try to minimalize."
  removeAndMinimalize original 4
  putStrLn "Remove 5 blocks from the original and try to minimalize."
  removeAndMinimalize original 5




assignment5 = do
    putStrLn "Generating NRC complient sudoku"
    start1 <- getCPUTime
    [r] <- rsolveNs [emptyN]
    showNode r
    end1   <- getCPUTime
    let diff1 = (fromIntegral (end1 - start1)) / (10^12)
    putStrLn ("   generating took: " ++ (show diff1) ++ " seconds")
    putStrLn "Generating minimalised problem"
    start2 <- getCPUTime
    s  <- genProblem r
    showNode s
    end2   <- getCPUTime
    let diff2 = (fromIntegral (end2 - start2)) / (10^12)
    putStrLn ("   minimizing took: " ++ (show diff2) ++ " seconds")
