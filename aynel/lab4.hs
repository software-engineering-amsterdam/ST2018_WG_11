module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- 1. Questions 'The Haskell Road' Chapter 4. Time: ..

-- 2. Random Data Generator

instance (Arbitrary a) => Arbitrary (Set a) where
    arbitrary = do
        list <- arbitrary
        return (Set list)

-- generate arbitrary :: IO (Set Int)

assignment2 = do
    generate arbitrary :: IO (Set (Int)