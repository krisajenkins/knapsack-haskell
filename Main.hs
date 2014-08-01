module Main where

import Data.List
import Data.Function (on)

type Weight = Integer
type Name = String

data Item = Item { name   :: Name
                 , weight :: Weight }
              deriving (Read, Show, Eq)

instance Ord Item where
  compare = compare `on` weight

items :: [Item]
items = [
           Item "Beatles" 12,
           Item "Bob Dylan" 23,
           Item "Bangles" 3,
           Item "Elton John" 9,
           Item "Blue" 1
        ]

weights :: [Item] -> Weight
weights = sum . map weight

------------------------------------------------------------

compareSolutions :: [Item] -> [Item] -> Ordering
compareSolutions xs ys
  | weights xs > weights ys = GT
  | weights xs < weights ys = LT
  | length xs > length ys = LT
  | length xs < length ys = GT
  | otherwise = EQ

largestSolution :: [[Item]] -> [Item]
largestSolution [] = []
largestSolution is = maximumBy compareSolutions is

knapsack :: ([Item] -> Item -> [Item]) -> [Item] -> Weight -> [Item]
knapsack _ [] _ = []
knapsack _ _ 0 = []
knapsack prune xs w = largestSolution possibleSolutions
  where validItems = filter (\i -> w >= weight i) xs
        possibleSolutions = fmap ksWithout validItems
        ksWithout i = i : knapsack prune (prune validItems i) (w - weight i)

-- | Unbounded Knapsack
uks :: [Item] -> Weight -> [Item]
uks = knapsack const

-- | Bounded Knapsack
bks :: [Item] -> Weight -> [Item]
bks = knapsack (flip delete)

-- TODO : Memoize both solutions.
main :: IO ()
main = do
         print (weights b, b)
         print (weights u, u)
         where n = 40
               b = bks items n
               u = uks items n
