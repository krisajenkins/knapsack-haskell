module Main where

import Data.List
import Data.Function (on)

type Weight = Integer
type Name = String

data Item = Item Name Weight
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

weight :: Item -> Weight
weight (Item _ w) = w

weights :: [Item] -> Weight
weights = sum . map weight

name :: Item -> Name
name (Item n _) = n

------------------------------------------------------------

compareSolutions :: [Item] -> [Item] -> Ordering
compareSolutions xs ys
  | weights xs > weights ys = GT
  | weights xs < weights ys = LT
  | length xs > length ys = LT
  | length xs < length ys = GT
  | otherwise = EQ

maxWeight :: Weight -> Item -> Bool
maxWeight w i = (>=) w (weight i)

largestSolution :: [[Item]] -> [Item]
largestSolution [] = []
largestSolution is = maximumBy compareSolutions is

-- | Unbounded Knapsack
uks :: [Item] -> Weight -> [Item]
uks [] _ = []
uks _ 0  = []
uks xs w = largestSolution possibleSolutions
                where validItems = filter (maxWeight w) xs
                      possibleSolutions = fmap uksWithout validItems
                      uksWithout i = i : uks validItems (w - weight i)

-- | Bounded Knapsack
bks :: [Item] -> Weight -> [Item]
bks [] _ = []
bks _ 0  = []
bks xs w = largestSolution possibleSolutions
                where validItems = filter (maxWeight w) xs
                      possibleSolutions = fmap bksWithout validItems
                      bksWithout i = i : bks (delete i validItems) (w - weight i)

-- TODO : Memoize both solutions.
main :: IO ()
main = do
         print (weights b, b)
         print (weights u, u)
         where n = 40
               b = bks items n
               u = uks items n
