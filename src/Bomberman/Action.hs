--------------------------------------------------------------------------------

module Bomberman.Action where

--------------------------------------------------------------------------------

import           Data.Board

--------------------------------------------------------------------------------

data Action
  = Action
  { actionMove :: Move
  , actionBomb :: Maybe Bomb
  } deriving (Show, Eq)

data Bomb
  = BombBeforeMove
  | BombAfterMove
  deriving (Show, Eq)

data Move
  = Stay
  | Move Direction
  deriving (Show, Eq)

--------------------------------------------------------------------------------

stay :: Action
stay = Action Stay Nothing

stayAndBomb :: Action
stayAndBomb = Action Stay $ Just BombAfterMove

bombAndStay :: Action
bombAndStay = Action Stay $ Just BombBeforeMove

move :: Direction -> Action
move = flip Action Nothing . Move

moveAndBomb :: Direction -> Action
moveAndBomb = flip Action (Just BombAfterMove) . Move

bombAndMove :: Direction -> Action
bombAndMove = flip Action (Just BombBeforeMove) . Move

--------------------------------------------------------------------------------
