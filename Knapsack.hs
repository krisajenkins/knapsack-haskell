module Knapsack where

import Data.Function.Memoize
import Data.List
import Data.Ord
import Util

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
