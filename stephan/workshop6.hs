data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

example1 = T 1 [T 2 [], T 3 []]
example2 = T 0 [example1,example1,example1]

-- q3
count :: Tree a -> Int
count (T _ []) = 1
count (T _ xs) = 1 + sum (map count xs)

-- q4
depth :: Tree a -> Int
depth (T _ []) = 0
depth (T _ xs) = 1 + (maximum (map depth xs))

-- q5
mapT :: (a -> b) -> Tree a -> Tree b
mapT f (T x xs) = T (f x) (map (mapT f) xs)

-- q6
{-
    ?
-}

-- q7
collect :: Tree a -> [a]
collect (T x xs) = [x] ++ concat (map collect xs)

foldT :: (a -> [b] -> b) -> Tree a -> b
foldT f (T x ts) = f x (map (foldT f) ts)

-- q8
countF :: Tree a -> Int
countF = foldT (\x xs -> 1 + sum xs)

depthF :: Tree a -> Int
depthF = foldT (\x xs -> 1 + maximum xs)

mapTF :: (a -> b) -> Tree a -> Tree b
mapTF f = foldT (\x xs -> T (f x) xs)

collectF :: Tree a -> [a]
collectF = foldT (\x xs -> [x] ++ concat xs)