module Workshop6 where

import Data.Char
import Data.List

data Blt a = Leaf a | Node (Blt a) (Blt a) deriving (Eq,Show)

exampleTree :: Blt String
exampleTree = Node (Node (Leaf "Hoare, Tony")
                         (Leaf "Turing, Alan"))
                         (Leaf "Goedel, Kurt")

-- Exercise 1
leafCount :: Blt a -> Int
leafCount (Leaf x) = 1
leafCount (Node x y) = leafCount x + leafCount y

-- Exercise 2
mapB :: (a -> b) -> Blt a -> Blt b
mapB f (Leaf x) = Leaf (f x)
mapB f (Node x y) = Node (mapB f x) (mapB f y)


data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

example1 = T 1 [T 2 [], T 3 []]
example2 = T 0 [example1,example1,example1]

countTree :: Tree a -> Int
countTree (T x []) = 1
countTree (T x xs) = 1 + sum (map countTree xs)

depthTree :: Tree a -> Int
depthTree (T x []) = 1
depthTree (T x xs) = 1 + (maximum (map depthTree xs))

mapT :: (a -> b) -> Tree a -> Tree b
mapT f (T x xs) = T (f x) (map (mapT f) xs)

collectTree :: Tree a -> [a]
collectTree (T x []) = [x]
collectTree (T x xs) = [x] ++ (concat (map collectTree xs))

foldT :: (a -> [b] -> b) -> Tree a -> b
foldT f (T x ts) = f x (map (foldT f) ts)

countTree' :: Tree a -> Int
countTree' tree = foldT (\x ys -> 1+sum ys) tree

depthTree' :: Tree a -> Int
depthTree' tree = foldT (\x ys -> 1+ maximum ys) tree