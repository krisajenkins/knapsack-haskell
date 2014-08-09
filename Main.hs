module Main where

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

------------------------------------------------------------
-- The knapsack algorithm
------------------------------------------------------------

type Cost a = a -> Integer

totalCost :: Cost a -> [a] -> Integer
totalCost cost = sum . fmap cost

costWithin :: Cost a -> Integer -> a -> Bool
costWithin cost w a = w >= cost a

largestSolution :: Cost a -> [[a]] -> [a]
largestSolution _ [] = []
largestSolution cost is = maximumBy (tiebreak (comparing (totalCost cost)) (flip (comparing length))) is

-- TODO : Memoize.
knapsack :: Cost a -> (a -> [a] -> [a]) -> Integer -> [a] -> [a]
knapsack _ _ 0 _  = []
knapsack _ _ _ [] = []
knapsack cost prune w xs = largestSolution cost possibleSolutions
  where validItems = filter (costWithin cost w) xs
        possibleSolutions = fmap knapsackWithout validItems
        knapsackWithout i = i : knapsack cost prune (w - cost i) (prune i validItems)

-- | Unbounded Knapsack. There is an unlimited number of each item.
uks :: Cost a -> Integer -> [a] -> [a]
uks cost = knapsack cost (flip const)

-- | Bounded Knapsack. Items can only be consumed once.
bks :: (Eq a) => Cost a -> Integer -> [a] -> [a]
bks cost = knapsack cost delete

------------------------------------------------------------
-- Items to put in the knapsack
------------------------------------------------------------

data Item = Item String Integer
              deriving (Read, Show, Eq)

itemCost :: Cost Item
itemCost (Item _ w) = w

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
  print (totalCost itemCost b, b)
  print (totalCost itemCost u, u)
  where n = 30
        b = bks itemCost n items
        u = uks itemCost n items
