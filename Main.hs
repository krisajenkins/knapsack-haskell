module Main where

import Data.List
import Data.Maybe

type Weight = Integer
type Name = String

data Item = Item Name Weight
              deriving (Read, Show, Eq)

instance Ord Item where
  compare a b = compare (weight a) (weight b)

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

name :: Item -> Name
name (Item n _) = n

maxMaybe :: Ord a => [a] -> Maybe a
maxMaybe [] = Nothing
maxMaybe xs = Just (maximum xs)

maxWeight :: Weight -> Item -> Bool
maxWeight w i = (>=) w (weight i)

knapsack :: [Item] -> Weight -> Weight
knapsack [] _ = 0
knapsack _ 0 = 0
knapsack xs w = fromMaybe 0 (maxMaybe possibleSolutions)
                where validItems = filter (maxWeight w) xs
                      possibleSolutions = fmap f validItems
                      f i = weight i + knapsack (delete i validItems) (w - weight i)

main :: IO ()
main = print (knapsack items 27)
