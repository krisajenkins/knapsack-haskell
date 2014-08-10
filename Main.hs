{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Function.Memoize
import Data.List
import Data.Ord

------------------------------------------------------------
-- Utility.
------------------------------------------------------------

-- | Compare with the first function. In the event of a tiebreak, compare with the next.
-- | I expect there's a built-in for this, but I can't find it.
tiebreak :: (a -> a -> Ordering) -> (a -> a -> Ordering) -> a -> a -> Ordering
tiebreak f g a b
  | f a b == EQ = g a b
  | otherwise = f a b

-- This shouldn't be list-specific, should it? It should work with any
-- functor. But sum seems unhappy with that idea.
totalWith :: (a -> Integer) -> [a] -> Integer
totalWith f = sum . fmap f

------------------------------------------------------------
-- The knapsack algorithm
------------------------------------------------------------

class Costed a where
  cost :: a -> Integer

type Solution a = [a]
type Pruner a = a -> [a] -> [a]
type Knapsacker a = Integer -> [a] -> Solution a

costsAtMost :: Costed a => Integer -> a -> Bool
costsAtMost w a = w >= cost a

compareSolutions :: Costed a => Solution a -> Solution a -> Ordering
compareSolutions = tiebreak (comparing (totalWith cost)) (flip (comparing length))

largestSolution :: Costed a => [Solution a] -> Solution a
largestSolution [] = []
largestSolution is = maximumBy compareSolutions is

validItems :: Costed a => Integer -> [a] -> [a]
validItems w xs = filter (costsAtMost w) xs

knapsack :: Costed a => Pruner a -> Knapsacker a -> Knapsacker a
knapsack _ _ 0 _ = []
knapsack _ _ _ [] = []
knapsack prune f w xs = largestSolution (fmap subSack (validItems w xs))
  where subSack i = i:is
          where w' = w - cost i
                xs' = prune i xs
                is = f w' xs'

-- | Unbounded Knapsack. There is an unlimited number of each item.
uks :: (Costed a, Memoizable a) => Knapsacker a
uks = memoFix2 (knapsack (flip const))

-- | Bounded Knapsack. Items can only be consumed once.
bks :: (Costed a, Memoizable a, Eq a) => Knapsacker a
bks = memoFix2 (knapsack delete)

------------------------------------------------------------
-- Items to put in the knapsack
------------------------------------------------------------

data Item = Item String Integer
              deriving (Read, Show, Eq)

name :: Item -> String
name (Item n _) = n

weight :: Item -> Integer
weight (Item _ w) = w

instance Ord Item where
  compare = tiebreak (comparing name) (comparing weight)

instance Costed Item where
  cost (Item _ w) = w

deriveMemoizable ''Item

items :: [Item]
items = [
          Item "Beatles" 12,
          Item "Bob Dylan" 23,
          Item "Bangles" 3,
          Item "Elton John" 9,
          Item "Blue" 1
        ]

 ------------------------------------------------------------

main :: IO ()
main = do
  print (totalWith cost b, b)
  print (totalWith cost u, u)
  where n = 100
        b = bks n items
        u = uks n items
