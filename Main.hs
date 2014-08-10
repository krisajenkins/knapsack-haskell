{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Function
import Data.Function.Memoize
import Data.List
import Data.Ord
import Debug.Trace

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

-- TODO Use this aliases.
type Solution a = [a]
type Prune a = a -> [a] -> [a]

costsAtMost :: Costed a => Integer -> a -> Bool
costsAtMost w a = w >= cost a

compareSolutions :: Costed a => Solution a -> Solution a -> Ordering
compareSolutions = tiebreak (comparing (totalWith cost)) (flip (comparing length))

largestSolution :: Costed a => [Solution a] -> Solution a
largestSolution [] = []
largestSolution is = maximumBy compareSolutions is

validItems :: Costed a => Integer -> [a] -> [a]
validItems w xs = filter (costsAtMost w) xs

--type KS a = Prune a -> Integer -> Set a -> Solution a
-- TODO : Memoize.
knapsack :: (Costed a, Show a) => Prune a -> (Integer -> [a] -> Solution a) -> Integer -> [a] -> Solution a
knapsack _ _ 0 _ = []
knapsack _ _ _ [] = []
knapsack prune f w xs = largestSolution (fmap knapsackWithout2 (validItems w xs))
  where knapsackWithout2 i = (:) i is
          where w' = w - cost i
                xs' = prune i xs
                is = f w' xs'

-- | Unbounded Knapsack. There is an unlimited number of each item.
uks :: (Costed a, Memoizable a, Show a) => Integer -> [a] -> Solution a
uks = fix (memoize2 . f)
      where f = knapsack (flip const)

-- | Bounded Knapsack. Items can only be consumed once.
bks :: (Costed a, Memoizable a, Eq a, Show a) => Integer -> [a] -> Solution a
bks = fix (memoize2 . f)
      where f = knapsack delete

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
  where n = 40
        b = bks n items
        u = uks n items
