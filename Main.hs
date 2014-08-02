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

class Costed a where
  cost :: a -> Integer

totalCost :: Costed a => [a] -> Integer
totalCost = sum . fmap cost

costWithin :: Costed a => Integer -> a -> Bool
costWithin w a = w >= cost a

largestSolution :: Costed a => [[a]] -> [a]
largestSolution [] = []
largestSolution is = maximumBy (tiebreak (comparing totalCost) (flip (comparing length))) is

-- TODO : Memoize.
knapsack :: Costed a => ([a] -> a -> [a]) -> Integer -> [a] -> [a]
knapsack _ 0 _  = []
knapsack _ _ [] = []
knapsack prune w xs = largestSolution possibleSolutions
                                  where validItems = filter (costWithin w) xs
                                        possibleSolutions = fmap knapsackWithout validItems
                                        knapsackWithout i = i : knapsack prune (w - cost i) (prune validItems i)

-- | Unbounded Knapsack
uks :: Costed a => Integer -> [a] -> [a]
uks = knapsack const

-- | Bounded Knapsack
bks :: (Eq a, Costed a) => Integer -> [a] -> [a]
bks = knapsack (flip delete)

------------------------------------------------------------
-- Items to put in the knapsack
------------------------------------------------------------

data Item = Item String Integer
              deriving (Read, Show, Eq)

instance Costed Item where
  cost (Item _ w) = w

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
         print (totalCost b, b)
         print (totalCost u, u)
         where n = 30
               b = bks n items
               u = uks n items
