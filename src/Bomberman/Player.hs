--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Bomberman.Player where

--------------------------------------------------------------------------------

import           Bomberman.Action
import           Bomberman.Cell
import           Data.Board
import           Data.Board.Path
import           Data.Monoid.Bool
import           Util.Random

--------------------------------------------------------------------------------

import           Control.Comonad
import           RIO
import qualified RIO.List         as L
import qualified RIO.Set          as Set

--------------------------------------------------------------------------------

player :: ( MonadIO m
          , MonadReader r m
          , HasLogFunc r
          )
       => Board Cell
       -> m Action
player board = do
  logInfo $ display board
  let markedBoard = extend mark $ narrow 11 board
      directions  = if isSafe . extract $ markedBoard
                    then directionsToTargets markedBoard
                    else escapeDirections markedBoard
  direction <- randomElement directions
  let bomb = shouldBomb markedBoard direction
  pure $ maybe (Action Stay bomb) (flip Action bomb . Move) direction

--------------------------------------------------------------------------------

directionsToTargets :: Board MarkedCell -> [Direction]
directionsToTargets board
  = Set.toList
  . Set.fromList
  . filter (willSurvive board)
  . mapMaybe L.headMaybe
  $ allShortest hasTargets isEmpty board

--------------------------------------------------------------------------------

escapeDirections :: Board MarkedCell -> [Direction]
escapeDirections board
  | L.null safeMoves && isSafeToStay = []
  | not . L.null $ safeMoves         = Set.toList . Set.fromList
                                       . filter (willSurvive board)
                                       $ mapMaybe L.headMaybe safeMoves
  | otherwise                        = fst <$> possibleMoves
  where around = neighbours board
        safeMoves = allShortest isSafe isEmpty board
        possibleMoves = L.filter (isEmpty . snd) around
        isSafeToStay = isSafe $ extract board

willDie :: Board MarkedCell -> Direction -> Bool
willDie dir board = (L.any ((== BombTimer1) .| isMonster) . getDangers . extract) $ shift board dir

willSurvive :: Board MarkedCell -> Direction -> Bool
willSurvive dir = not . willDie dir

--------------------------------------------------------------------------------

shouldBomb :: Board MarkedCell -> Maybe Direction -> Bomb
shouldBomb _ Nothing        = BombBeforeMove
shouldBomb board (Just dir) =
  case shortest (not . getSelfVisible) isEmpty board of
    Nothing -> bombWhen gain
    _       ->  case shortest isSafe isEmpty board' of
      Nothing -> NoBomb
      Just  p -> bombWhen $ length p < 4 && gain
  where board'
          = extend mark
          . setFocus Bomberman
          . shift dir
          . setFocus BombTimer5
          . fmap getCell
          $ board
        gain = hasTargets . extract $ board
        bombWhen v = if v then BombBeforeMove else NoBomb

--------------------------------------------------------------------------------

data MarkedCell
  = MarkedCell
  { getCell        :: !Cell
  , getDangers     :: ![Cell]
  , getTargets     :: ![Cell]
  , getNeigbours   :: ![(Direction, Cell)]
  , getSelfVisible :: !Bool
  } deriving (Show)

instance Display MarkedCell where
  display markedCell
    | isEmpty markedCell && not (isSafe markedCell) = "†"
    | otherwise = display (getCell markedCell)

mark :: Board Cell -> MarkedCell
mark board = MarkedCell (extract board) dangers saneTargets (neighbours board) self
  where dangers = bombs <> monsters
        bombs = filter isBomb (allVisible (not . isAnyWall) 3 board)
        monsters = filter (isMonster .| isEnemy) (allVisible (not . isAnyWall) 1 board)
        targets
          = filter (isDestroyableWall .| isMonster .| isEnemy)
          $ allVisible (not . isWall) 3 board
        saneTargets = if L.null bombs then targets else []
        self = not . L.null . filter isBomberman $ allVisible (not . isAnyWall) 3 board

isEmpty :: MarkedCell -> Bool
isEmpty = (== Empty) . getCell

isSafe :: MarkedCell -> Bool
isSafe = L.null . getDangers

hasTargets :: MarkedCell -> Bool
hasTargets = not . L.null . getTargets

--------------------------------------------------------------------------------
