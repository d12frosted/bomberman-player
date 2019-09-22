--------------------------------------------------------------------------------

-- | Universe representation.
--
-- The universe is an interesting thing by itself. Most philosophers view it as
-- an endless strip. But the universe itself doesn't make any sense without
-- observation. Some think that there is a subject that observes the universe
-- and moves around. But we (you and me) know that it's the universe observing
-- the subject. Universe is focused on the subject. Subject doesn't move, it's the
-- universe which moves and adapts.
--
-- The universe is a triplet of the subject, everything on the 'left' of the
-- subject, and everything on the 'right'. In the most trivial case, it's a 1-D
-- universe. But imagine that universe consists of other universes. Now you have universe of
-- N dimensions.
--
-- Let the fun begin.
--

--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}

--------------------------------------------------------------------------------

module Data.Universe
  ( Universe(..)
  , mkUniverse
  , setFocus
  , left
  , maybeLeft
  , right
  , maybeRight
  , narrow
  , sequenceU
  ) where

--------------------------------------------------------------------------------

import           Control.Comonad
import           RIO
import           RIO.List         (iterate)
import           RIO.List.Partial (tail, (!!))

--------------------------------------------------------------------------------

data Universe a = Universe [a] a [a]
  deriving (Eq, Show, Functor)

--------------------------------------------------------------------------------

-- | Create a 'Universe' focused on i-th element of the input list.
mkUniverse :: Int -> [a] -> Universe a
mkUniverse foc as
  = Universe (reverse . take l $ as) (as !! p) (take r . drop (l + 1) $ as)
  where n = length as
        p = min (n - 1) . max foc $ 0
        r = n - p - 1
        l = n - r - 1

--------------------------------------------------------------------------------

instance Comonad Universe where
  extract (Universe _ x _) = x
  duplicate u = Universe (tail $ iterate left u) u (tail $ iterate right u)

--------------------------------------------------------------------------------

setFocus :: a -> Universe a -> Universe a
setFocus a (Universe ls _ rs) = Universe ls a rs

--------------------------------------------------------------------------------

left :: Universe a -> Universe a
left w = fromMaybe w $ maybeLeft w

maybeLeft :: Universe a -> Maybe (Universe a)
maybeLeft (Universe []     _ _)  = Nothing
maybeLeft (Universe (l:ls) a rs) = Just $ Universe ls l (a:rs)

right :: Universe a -> Universe a
right w = fromMaybe w $ maybeRight w

maybeRight :: Universe a -> Maybe (Universe a)
maybeRight (Universe _  _ [])     = Nothing
maybeRight (Universe ls a (r:rs)) = Just $ Universe (a:ls) r rs

--------------------------------------------------------------------------------

sequenceU :: Universe (Maybe (Universe a)) -> Maybe (Universe (Universe a))
sequenceU (Universe ls w rs) = case (sequenceA ls, w, sequenceA rs) of
  (Just l, Just m, Just r) -> Just $ Universe l m r
  _                        -> Nothing

--------------------------------------------------------------------------------

narrow :: Int -> Universe a -> Universe a
narrow r (Universe ls a rs) = Universe (take r ls) a (take r rs)

--------------------------------------------------------------------------------
