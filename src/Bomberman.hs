--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------------------------------------

module Bomberman
  ( module M
  , parseBoard
  ) where

--------------------------------------------------------------------------------

import           Bomberman.Action as M
import           Bomberman.Cell   as M
import           Bomberman.Player as M
import           Data.Board

--------------------------------------------------------------------------------

import           RIO

--------------------------------------------------------------------------------

parseBoard :: Int -> Text -> Board Cell
parseBoard = parse parseCell isBomberman

--------------------------------------------------------------------------------
