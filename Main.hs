{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Function.Memoize
import Data.Ord
import Util
import Knapsack

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
          Item "Blue" 2
        ]

 ------------------------------------------------------------

main :: IO ()
main = do
  print (totalWith cost b, b)
  print (totalWith cost u, u)
  where n = 45
        b = bks n items
        u = uks n items
