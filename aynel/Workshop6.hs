module Workshop6 where
import Data.Char
import Data.List

data Blt a = Leaf a | Node (Blt a) (Blt a) deriving (Eq,Show)

exampleTree :: Blt String
exampleTree = Node (Node (Leaf "Hoare, Tony")
                        (Leaf "Turing, Alan"))
                   (Leaf "Goedel, Kurt")

--  1. Counts the number of leaf nodes in a binary tree
leafCount :: Blt a -> Int
leafCount (Leaf a)          = 1
leafCount (Node left right) = 1 + leafCount left + leafCount right

-- 2. mapB that does for binary trees what map does for lists
mapB :: (a -> b) -> Blt a -> Blt b
mapB f (Leaf a)          = Leaf (f a)
mapB f (Node left right) = Node (mapB f left) (mapB f right)

-- definition of trees with an arbitrary number of branches, and information at the internal nodes
data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

example1 = T 1 [T 2 [], T 3 []]
example2 = T 0 [example1,example1,example1]

-- count :: Tree a -> Int
