module Util where

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
