--------------------------------------------------------------------------------

module Bomberman.Action where

--------------------------------------------------------------------------------

import           Data.Board

--------------------------------------------------------------------------------

data Action
  = Action
  { actionMove :: Move
  , actionBomb :: Bomb
  } deriving (Show, Eq)

data Bomb
  = NoBomb
  | BombBeforeMove
  | BombAfterMove
  deriving (Show, Eq)

data Move
  = Stay
  | Move Direction
  deriving (Show, Eq)

--------------------------------------------------------------------------------

stay :: Action
stay = Action Stay NoBomb

stayAndBomb :: Action
stayAndBomb = Action Stay BombAfterMove

bombAndStay :: Action
bombAndStay = Action Stay BombBeforeMove

move :: Direction -> Action
move = flip Action NoBomb . Move

moveAndBomb :: Direction -> Action
moveAndBomb = flip Action BombAfterMove . Move

bombAndMove :: Direction -> Action
bombAndMove = flip Action BombBeforeMove . Move

--------------------------------------------------------------------------------
