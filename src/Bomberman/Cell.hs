--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Bomberman.Cell where

--------------------------------------------------------------------------------

import           RIO
import           RIO.Text (unpack)

--------------------------------------------------------------------------------

data Cell
  = Bomberman          -- ☺
  | BombBomberman      -- ☻
  | DeadBomberman      -- Ѡ

  | OtherBomberman     -- ♥
  | OtherBombBomberman -- ♠
  | OtherDeadBomberman -- ♣

  | BombTimer5         -- 5
  | BombTimer4         -- 4
  | BombTimer3         -- 3
  | BombTimer2         -- 2
  | BombTimer1         -- 1
  | Boom               -- ҉

  | Wall               -- ☼
  | DestroyableWall    -- #
  | DestroyedWall      -- H

  | MeatChopper        -- &
  | DeadMeatChopper    -- x

  | Empty              -- ' '
  deriving (Eq)

--------------------------------------------------------------------------------

instance Show Cell where
  show = unpack . textDisplay

instance Display Cell where
  display Bomberman          = "@"
  display BombBomberman      = "!"
  display DeadBomberman      = "x"
  display OtherBomberman     = "b"
  display OtherBombBomberman = "B"
  display OtherDeadBomberman = "p"
  display BombTimer5         = "5"
  display BombTimer4         = "4"
  display BombTimer3         = "3"
  display BombTimer2         = "2"
  display BombTimer1         = "1"
  display Boom               = "0"
  display Wall               = "▪"
  display DestroyableWall    = "▫"
  display DestroyedWall      = "·"
  display MeatChopper        = "m"
  display DeadMeatChopper    = "M"
  display Empty              = "."

--------------------------------------------------------------------------------

parseCell :: Char -> Cell
parseCell '@' = Bomberman
parseCell '!' = BombBomberman
parseCell 'x' = DeadBomberman
parseCell 'b' = OtherBomberman
parseCell 'B' = OtherBombBomberman
parseCell 'p' = OtherDeadBomberman
parseCell '5' = BombTimer5
parseCell '4' = BombTimer4
parseCell '3' = BombTimer3
parseCell '2' = BombTimer2
parseCell '1' = BombTimer1
parseCell '0' = Boom
parseCell '▪' = Wall
parseCell '▫' = DestroyableWall
parseCell '·' = DestroyedWall
parseCell 'm' = MeatChopper
parseCell 'M' = DeadMeatChopper
parseCell _   = Empty

--------------------------------------------------------------------------------

isAnyWall :: Cell -> Bool
isAnyWall Wall            = True
isAnyWall DestroyableWall = True
isAnyWall _               = False

isWall :: Cell -> Bool
isWall Wall = True
isWall _    = False

isDestroyableWall :: Cell -> Bool
isDestroyableWall DestroyableWall = True
isDestroyableWall _               = False

isMonster :: Cell -> Bool
isMonster MeatChopper = True
isMonster _           = False

isEnemy :: Cell -> Bool
isEnemy OtherBomberman     = True
isEnemy OtherBombBomberman = True
isEnemy _                  = False

isBomberman :: Cell -> Bool
isBomberman Bomberman     = True
isBomberman BombBomberman = True
isBomberman DeadBomberman = True
isBomberman _             = False

isBomb :: Cell -> Bool
isBomb BombTimer5 = True
isBomb BombTimer4 = True
isBomb BombTimer3 = True
isBomb BombTimer2 = True
isBomb BombTimer1 = True
isBomb _          = False

--------------------------------------------------------------------------------
