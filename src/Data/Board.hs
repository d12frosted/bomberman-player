--------------------------------------------------------------------------------

-- | Definitely not interesting two-dimensional example of the 'Universe'.

--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Data.Board where

--------------------------------------------------------------------------------

import           Data.List.Extra
import           Data.Universe    as U

--------------------------------------------------------------------------------

import           Control.Comonad
import           GHC.Generics
import           RIO
import           RIO.List         (findIndex, intersperse, iterate)
import qualified RIO.List         as L
import           RIO.List.Partial (tail)
import           RIO.Text         (unpack)

--------------------------------------------------------------------------------

-- | Board is a two-dimensional universe.
newtype Board a
  = Board
  { getUniverse :: Universe (Universe a)
  } deriving (Functor)

instance Comonad Board where
  extract = extract . extract . getUniverse
  -- For convenience of the 'extend' usage, 'duplicate' doesn't produce infinite
  -- 'Board'.
  duplicate = fmap Board . Board . shifted . shifted . getUniverse
    where shifted :: Universe (Universe a) -> Universe (Universe (Universe a))
          shifted u@(Universe _ (Universe ls _ rs) _) = Universe
            (take (length ls) . tail $ iterate (fmap left) u)
            u
            (take (length rs) . tail $ iterate (fmap right) u)

--------------------------------------------------------------------------------

instance Display a => Display (Board a) where
  display (Board (Universe ls m rs))
    = mconcat
    [ " "
    , mconcat $ intersperse "\n " (displayStrip "[" "]" <$> reverse ls)
    , "\n["
    , displayStrip " " " " m
    , "]\n "
    , mconcat $ intersperse "\n " (displayStrip "[" "]" <$> rs)
    ]
    where displayStrip lsep rsep (Universe ls' a rs')
            = mconcat
            [ mconcat (display <$> reverse ls')
            , lsep
            , display a
            , rsep
            , mconcat (display <$> rs')
            ]

--------------------------------------------------------------------------------

parse :: (Char -> a) -> (a -> Bool) -> Int -> Text -> Board a
parse parseCell isFocus width input
  = Board $ mkUniverse indexX <$> mkUniverse indexY matrix
  where cells  = parseCell <$> unpack input
        matrix = group width cells
        index  = fromMaybe 0 . findIndex isFocus $ cells
        indexX = index `mod` width
        indexY = index `div` width

--------------------------------------------------------------------------------

setFocus :: a -> Board a -> Board a
setFocus a (Board u) = Board (U.setFocus (U.setFocus a $ extract u) u)

--------------------------------------------------------------------------------

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq, Ord, Enum, Bounded, Generic, Show)

instance Hashable Direction

instance Display Direction where
  display North = "N"
  display South = "S"
  display East  = "E"
  display West  = "W"

allDirections :: [Direction]
allDirections = [minBound..maxBound]

--------------------------------------------------------------------------------

-- | Shift the board by 'Direction'. If the end of the 'Board' is encountered,
-- does nothing.
--
-- See 'maybeShift' for the variant that doesn't allow to shift past to borders.
shift :: Direction -> Board a -> Board a
shift North (Board u) = Board $ left      u
shift South (Board u) = Board $ right     u
shift East  (Board u) = Board $ right <$> u
shift West  (Board u) = Board $ left  <$> u

-- | Shift the board by 'Direction'. Returns shifted board iff the direction
-- doesn't lead out of the borders.
maybeShift :: Direction -> Board a -> Maybe (Board a)
maybeShift North (Board u) = Board <$> maybeLeft u
maybeShift South (Board u) = Board <$> maybeRight u
maybeShift East  (Board u) = Board <$> sequenceU (maybeRight <$> u)
maybeShift West  (Board u) = Board <$> sequenceU (maybeLeft  <$> u)

--------------------------------------------------------------------------------

neighbours :: Board a -> [(Direction, a)]
neighbours u
  = zip allDirections
  . catMaybes
  $ fmap (fmap extract . flip maybeShift u) allDirections

--------------------------------------------------------------------------------

narrow' :: Int -> Int -> Board a -> Board a
narrow' x y = Board . fmap (U.narrow x) . U.narrow y . getUniverse

narrow :: Int -> Board a -> Board a
narrow n = narrow' n n

--------------------------------------------------------------------------------

allVisible :: (a -> Bool) -> Int -> Board a -> [a]
allVisible blocker distance board
  = mconcat $ by <$> allDirections
  where by direction
          = fmap extract
          . catMaybes
          . take distance
          . tail
          . L.iterate (>>= mfilter (blocker . extract) . maybeShift direction)
          $ Just board

--------------------------------------------------------------------------------

anyVisible :: (a -> Bool) -> (a -> Bool) -> Int -> Board a -> Bool
anyVisible pred valid distance board
  = L.or $ by <$> allDirections
  where
    by direction
      = L.any pred
      . fmap extract
      . catMaybes
      . take distance
      . tail
      . L.iterate (>>= mfilter (valid . extract) . maybeShift direction)
      $ Just board

--------------------------------------------------------------------------------
