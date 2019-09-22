--------------------------------------------------------------------------------

-- | This module exposes functions for searching the closest (shortest) path to
-- a cell on a 'Board'.
--
-- The target cell is specified by a predicate and since the search is made on
-- generic 'Board' one must provide a predicate indicating if the cell is a
-- valid cell for a 'move'.
--
-- For example, board may contain coins that you are searching and walls
-- blocking the way.
--
-- >>> shortest isCoin (not . isWall) board
-- returns path to the closest coin avoiding walls.
--
-- For the efficiency reasons, this function doesn't calculate all possible
-- paths on the 'Board'.
--

--------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}

--------------------------------------------------------------------------------

module Data.Board.Path
  ( shortest
  , allShortest
  )
where

--------------------------------------------------------------------------------

import           Data.Board
import           Data.List.Extra
import           Data.Seq.Extra

----------------------------------------------------------------------

import           Control.Comonad
import           Data.Hashable
import           RIO
import qualified RIO.HashSet     as HS
import qualified RIO.List        as L
import           RIO.Seq         (Seq (..))
import qualified RIO.Seq         as Seq

--------------------------------------------------------------------------------

shortest :: (a -> Bool)       -- ^ is target cell
         -> (a -> Bool)       -- ^ is valid cell for move
         -> Board a           -- ^ board
         -> Maybe [Direction] -- ^ resulting directions
shortest pred valid
  = fmap (toList . fromPath)
  . L.minimumMaybe
  . paths pred valid

allShortest :: (a -> Bool)   -- ^ is target cell
            -> (a -> Bool)   -- ^ is valid cell for move
            -> Board a       -- ^ board
            -> [[Direction]] -- ^ resulting directions
allShortest pred valid
  = fmap fromPath
  . allMinimum
  . toList
  . paths pred valid

--------------------------------------------------------------------------------

data Path
  = Path
  { pathReversed :: ![Direction]
  , pathCoord    :: !Coord
  , pathFound    :: !Bool
  } deriving Show

fromPath :: Path -> [Direction]
fromPath = reverse . pathReversed

instance Hashable Path where
  hashWithSalt v = hashWithSalt v . pathReversed

instance Eq Path where
  p1 == p2 = pathCoord p1 == pathCoord p2

instance Ord Path where
  compare p1 p2 = compareLength (pathReversed p1) (pathReversed p2)

emptyPath :: Path
emptyPath = Path [] (coord 0 0) False

(+>) :: Path -> Direction -> Path
path +> d
  = path
  { pathReversed = d : pathReversed path
  , pathCoord = add (coordinate d) (pathCoord path)
  }

--------------------------------------------------------------------------------

newtype Coord = Coord (Int, Int) deriving (Show, Eq, Hashable)

coord :: Int -> Int -> Coord
coord x y = Coord (x, y)

add :: Coord -> Coord -> Coord
add (Coord (x1, y1)) (Coord (x2, y2)) = coord (x1 + x2) (y1 + y2)

--------------------------------------------------------------------------------

coordinate :: Direction -> Coord
coordinate North = coord 0      1
coordinate South = coord 0    (-1)
coordinate West  = coord (-1)   0
coordinate East  = coord 1      0

--------------------------------------------------------------------------------

paths :: (a -> Bool) -- ^ is target cell
      -> (a -> Bool) -- ^ is valid cell for move
      -> Board a
      -> [Path]
paths !target !valid !board
  = toList . HS.fromList . toList . Seq.filter pathFound
  $ go target valid mempty mempty [(board, emptyPath)]

-- We are using global visited set, because we search only for shortest paths
-- instead of all possible, so it increases performance tremendously. If you
-- wish to search for all possible, move the visited set into the 'Right' state
-- (e.g. Seq (Board a, Path, HashSet Coord))
go :: (a -> Bool)
   -> (a -> Bool)
   -> HashSet Coord
   -> Seq Path
   -> Seq (Board a, Path)
   -> Seq Path
go target valid visited ls rs =
  case splitEithers . seqMaybes $ step target valid visited <$> rs <*> allDirs of
    (ls', Seq.Empty) -> ls <> ls'
    (ls', rs')       ->
      let visited' = HS.union visited (HS.fromList . toList $ (pathCoord . snd <$> rs'))
      in go target valid visited' (ls <> ls') rs'

step :: (a -> Bool)
     -> (a -> Bool)
     -> HashSet Coord
     -> (Board a, Path)
     -> Direction
     -> Maybe (Either Path (Board a, Path))
step target valid visited (board, path) dir
  | pathFound path = Just . Left $ path
  | HS.member (pathCoord $ path +> dir) visited ||
    maybe True (not . valid . extract) (maybeShift dir board) = Nothing
  | otherwise = let newPath = path +> dir
                    newBoard = shift dir board
                in Just . Right $ ( newBoard
                                  , newPath { pathFound = target . extract $ newBoard}
                                  )

--------------------------------------------------------------------------------

allDirs :: Seq Direction
allDirs = Seq.fromList allDirections

--------------------------------------------------------------------------------
