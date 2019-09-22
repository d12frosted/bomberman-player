--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------------------------------------

module Data.Seq.Extra where

--------------------------------------------------------------------------------

import           RIO
import           RIO.Seq (Seq (..))
import qualified RIO.Seq as Seq

--------------------------------------------------------------------------------

seqMaybes :: Seq (Maybe a) -> Seq a
seqMaybes = fmap (fromMaybe undefined) . Seq.filter isJust

splitEithers :: Seq (Either a b) -> (Seq a, Seq b)
splitEithers = foldr (either left right) (Seq.Empty, Seq.Empty)
 where
  left  a ~(l, r) = (a :<| l, r)
  right a ~(l, r) = (l, a :<| r)

--------------------------------------------------------------------------------
