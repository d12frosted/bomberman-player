-- | Extras for '[]'.

--------------------------------------------------------------------------------

module Data.List.Extra
  ( group
  , stripPrefix
  , allMaximum
  , allMaximumBy
  , allMinimum
  , allMinimumBy
  , compareLength
  ) where

--------------------------------------------------------------------------------

import           Data.Maybe (fromMaybe)
import qualified RIO.List   as L

--------------------------------------------------------------------------------

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative or zero n" -- lulz, we are totally unsafe

--------------------------------------------------------------------------------

stripPrefix :: Eq a => [a] -> [a] -> [a]
stripPrefix p l = fromMaybe l $ L.stripPrefix p l

--------------------------------------------------------------------------------

allMaximum :: Ord a => [a] -> [a]
allMaximum = allMaximumBy compare

allMaximumBy :: (a -> a -> Ordering) -> [a] -> [a]
allMaximumBy ord xs = case L.maximumByMaybe ord xs of
  Nothing -> []
  Just  v -> filter ((== EQ) . ord v) xs

allMinimum :: Ord a => [a] -> [a]
allMinimum = allMinimumBy compare

allMinimumBy :: (a -> a -> Ordering) -> [a] -> [a]
allMinimumBy ord xs = case L.minimumByMaybe ord xs of
  Nothing -> []
  Just  v -> filter ((== EQ) . ord v) xs

--------------------------------------------------------------------------------

compareLength :: Foldable t => t a -> t a -> Ordering
compareLength a b = compare (length a) (length b)

--------------------------------------------------------------------------------
