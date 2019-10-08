--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import           Bomberman
import           Data.Board
import           Data.Board.Path
import           Data.Monoid.Bool

--------------------------------------------------------------------------------

import           Control.Comonad
import           RIO
import qualified RIO.List         as L
import qualified RIO.List.Partial as L
import qualified RIO.Text         as T
import           Test.Tasty
import           Test.Tasty.HUnit

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  $ testGroup "Bomberman player"
  [ testGroup "Basic safety"
    [ testCase "Stay away from meat choppers" $
      escapeDirections ( mkBoard [ "▪m▪"
                                 , "▪.▪"
                                 , "▪@▪"
                                 , "▪▪▪"
                                 ]
                       ) @?= []

    , testCase "waiting for bomb to explode is ok" $
      escapeDirections ( mkBoard [ "▪▪▪▪▪"
                                 , "▪5..▪"
                                 , "▪▪▪@▪"
                                 , "▪▪▪▪▪"
                                 ]
                       ) @?= []

    , testCase "Stay when bombs are around" $
      escapeDirections ( mkBoard [ "▪▪▪▪▪"
                                 , "▪5.4▪"
                                 , "▪.@.▪"
                                 , "▪2.3▪"
                                 , "▪▪▪▪▪"
                                 ]
                       ) @?= []

    , testCase "do not jump to explosion, for real" $
      escapeDirections ( mkBoard [ "▪▫▪.▪."
                                 , ".m1.▫▫"
                                 , "▪.▪@▪."
                                 , "...▫.."
                                 , "▪.▪▫▪."
                                 ]
                       )  @?= []

    , testCase "Do not take route to safety if will be exploded on the way" $
      escapeDirections ( mkBoard [ "▪▪▪▪▪▪"
                                 , "▪▪4.3▪"
                                 , "▪..@.▪"
                                 , "▪▪1.2▪"
                                 , "▪▪▪▪▪▪"
                                 ]
                       ) @?= []

    , testCase "do not block yourself with a bomb - 0 - 1" $
      shouldBomb ( mkBoard [ "▪.▪.▪.▪.▪"
                           , "........."
                           , "▪.▪▫▪.▪.▪"
                           , "........."
                           , "▪.▪@▪.▪.▪"
                           , "..▫.▫...."
                           , "▪.▪▫▪.▪.▪"
                           ]
                 ) (Just South) @?= Nothing

    , testCase "do not block yourself with a bomb - 0 - 2" $
      shouldBomb ( mkBoard [ "▪.▪.▪.▪.▪"
                           , "........."
                           , "▪.▪.▪▫▪.▪"
                           , "...@..▫.."
                           , "▪.▪.▪▫▪.▪"
                           , "..▫.▫...."
                           , "▪.▪▫▪.▪.▪"
                           ]
                 ) (Just East) @?= Nothing

    , testCase "do not block yourself with a bomb - 1" $
      shouldBomb ( mkBoard [ "▪▪▪"
                           , "▪.▪"
                           , "▪@▪"
                           , "▪.▪"
                           , "▪▪▪"
                           ]
                 ) (Just North) @?= Nothing

    , testCase "do not block yourself with a bomb - 2" $
      shouldBomb ( mkBoard [ "▪▪▪"
                           , "▪.▪"
                           , "▪.▪"
                           , "▪@▪"
                           , "▪.▪"
                           , "▪.▪"
                           , "▪▪▪"
                           ]
                 ) (Just North) @?= Nothing

    , testCase "do not block yourself with a bomb - 3" $
      shouldBomb ( mkBoard [ "▪▪▪"
                           , "▪.▪"
                           , "▪.▪"
                           , "▪.▪"
                           , "▪@▪"
                           , "▪.▪"
                           , "▪.▪"
                           , "▪.▪"
                           , "▪▪▪"
                           ]
                 ) (Just North) @?= Nothing

    , testCase "do not block yourself with a bomb - 4" $
      shouldBomb ( mkBoard [ "▪▪▪"
                           , "▪.▪"
                           , "▪.▪"
                           , "▪.▪"
                           , "▪.▪"
                           , "▪@▪"
                           , "▪.▪"
                           , "▪.▪"
                           , "▪.▪"
                           , "▪.▪"
                           , "▪▪▪"
                           ]
                 ) (Just North) @?= Nothing

    , testCase "find path to safety" $
      shortest isSafe isEmpty ( mkBoard [ "▪▪▪▪"
                                        , "▪..▪"
                                        , "▪▪.▪"
                                        , "▪▪@▪"
                                        , "▪▪5▪"
                                        , "▪▪▪▪"
                                        ]
                              ) @?= Just [ North, North, West ]

    , testCase "find path to safety from bomb away - 1" $
      escapeDirections ( mkBoard [ "▪.▪.▪.▪.▪.▪▫▪.▪.▪.▪▫▪.▪.▪.▪.▪.▪.▪"
                                 , "▪..........@2..▫▫.▫.............▪"
                                 , "▪.▪.▪.▪.▪.▪▫▪.▪.▪.▪.▪▫▪.▪.▪.▪b▪▫▪"
                                 ]
                       ) @?= [ West ]
    , testCase "find path to safety from bomb away - 2" $
      escapeDirections ( mkBoard [ "▪.▪.▪.▪.▪.▪▫▪.▪.▪.▪▫▪.▪.▪.▪.▪.▪.▪"
                                 , "▪.........@.2..▫▫.▫.............▪"
                                 , "▪.▪.▪.▪.▪.▪▫▪.▪.▪.▪.▪▫▪.▪.▪.▪b▪▫▪"
                                 ]
                       ) @?= [ West ]
    , testCase "find path to safety from bomb away - 3" $
      escapeDirections ( mkBoard [ "▪.▪.▪.▪.▪.▪▫▪.▪.▪.▪▫▪.▪.▪.▪.▪.▪.▪"
                                 , "▪........@..2..▫▫.▫.............▪"
                                 , "▪.▪.▪.▪.▪.▪▫▪.▪.▪.▪.▪▫▪.▪.▪.▪b▪▫▪"
                                 ]
                       ) @?= [ North, South, West ]
    ]

  , testGroup "Path to safety with bomb awareness"
    [ testCase "do not stay on bomb" $
      escapeDirections ( mkBoard [ "▪▪▪▪"
                                 , "▪.!▪"
                                 , "▪▪▪▪"
                                 ]
                       ) @?= [ West ]

    , testCase "escape bomb when safety is close" $
      escapeDirections ( mkBoard [ "▪▪▪▪▪▪"
                                 , "▪▪4.3▪"
                                 , "▪.@..▪"
                                 , "▪▪2.2▪"
                                 , "▪▪▪▪▪▪"
                                 ]
                       ) @?= [East, West]

    , testCase "escape bomb when safety is far away" $
      escapeDirections ( mkBoard [ "▪▪▪▪"
                                 , "▪..▪"
                                 , "▪▪.▪"
                                 , "▪▪@▪"
                                 , "▪▪.▪"
                                 , "▪▪5▪"
                                 , "▪▪▪▪"
                                 ]
                       ) @?= [ North ]

    , testCase "move when you can, even from safety through fire" $
      escapeDirections ( mkBoard [ "▪▪▪▪▪▪"
                                 , "▪▪4.3▪"
                                 , "▪..@.▪"
                                 , "▪▪2.2▪"
                                 , "▪▪▪▪▪▪"
                                 ]
                       ) @?= [West]

    , testCase "escape bombs through other bombs" $
      escapeDirections ( mkBoard [ "▪▪▪▪▪▪▪▪"
                                 , "▪▪▪▪▪.▪▪"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪5...@5▪"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪▪▪▪"
                                 ]
                       ) @?= [North]

    , testCase "wait for other bomb to explode so path to safety is clear - 1" $
      escapeDirections ( mkBoard [ "▪▪▪▪▪▪▪▪"
                                 , "▪▪▪▪▪▪▪▪"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪1...@5▪"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪▪▪▪"
                                 ]
                       ) @?= [North]
    ]

  , testGroup "Hard decisions about death"
    [ testCase "prefer potential death from the monster" $
      escapeDirections ( mkBoard [ "▪▪▪.▪"
                                 , "▪.@m▪"
                                 , "▪1▪.▪"
                                 , "▪▪▪▪▪"
                                 ]
                       ) @?= []

    , testCase "take unsafe path if it's the only one" $
      escapeDirections ( mkBoard [ "▪▪▪"
                                 , "▪.▪"
                                 , "▪@▪"
                                 , "▪5▪"
                                 , "▪▪▪"
                                 ]
                       ) @?= [ North ]

    , testCase "accept your death in agony" $
      escapeDirections ( mkBoard [ "▪▪▪▪▪▪▪▪"
                                 , "▪▪▪▪▪▪▪▪"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪2...@5▪"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪..5"
                                 , "▪▪▪▪▪▪▪▪"
                                 ]
                       ) @?= [North, South, West]

    , testCase "don't move if there is nowhere to move" $
      escapeDirections ( mkBoard [ "▪▫▪"
                                 , "▫@▫"
                                 , "▪▫▪"
                                 ]
                       ) @?= []

    , testCase "bomb when blocked by brick walls with no way to move" $
      shouldBomb ( mkBoard [ "▪▫▪"
                           , "▫@▫"
                           , "▪▫▪"
                           ]
                 ) Nothing @?= Just BombBeforeMove

    , testCase "bomb when there are brick walls around and available area is small" $
      shouldBomb ( mkBoard [ "▪▫▪"
                           , "▫@▫"
                           , "▪.▪"
                           , "▪▪▪"
                           ]
                 ) (Just South) @?= Just BombBeforeMove

    , testCase "but do not bomb when there is nothing to explode even if the available are is small" $
      shouldBomb ( mkBoard [ "▪▪▪"
                           , "▫.▫"
                           , "▪@▪"
                           , "▪▪▪"
                           ]
                 ) (Just North) @?= Nothing
    ]

  , testGroup "game rules"
    [ testCase "wall stops the unsafe of bomb" $
      escapeDirections ( mkBoard [ "▪▪▪▪▪"
                                 , "▪5▪.▪"
                                 , "▪▪▪@▪"
                                 , "▪▪▪▪▪"
                                 ]
                       ) @?= [ North ]
    ]

  , testGroup "aggressiveness"
    [ testCase "destroy brick walls" $
      shouldBomb ( mkBoard [ "▪▪▪▪"
                           , "▪▫▪▪"
                           , "▪@.▪"
                           , "▪▪.▪"
                           , "▪▪.▪"
                           , "▪▪.▪"
                           , "▪▪.▪"
                           ]
                 ) (Just East) @?= Just BombBeforeMove

    , testCase "treat monsters as brick wall - destroy it" $
      shouldBomb ( mkBoard [ "▪▪▪▪"
                           , "▪m▪▪"
                           , "▪@.▪"
                           , "▪▪.▪"
                           ]
                 ) (Just East) @?= Just BombBeforeMove
    , testCase "treat other bombermans as brick wall - destroy it" $
      shouldBomb ( mkBoard [ "▪▪▪▪"
                           , "▪b▪▪"
                           , "▪@.▪"
                           , "▪▪.▪"
                           ]
                 ) (Just East) @?= Just BombBeforeMove


    , testCase "do not bomb when there are no targets around" $
      shouldBomb ( mkBoard [ "▪.▪.▪.▪.▪.▪"
                           , "..........."
                           , "▪.▪.▪▫▪.▪.▪"
                           , "..........."
                           , "▪.▪.▪.▪.▪.▪"
                           , "..........."
                           , "▪.▪b▪@▪▫▪.▪"
                           , "....▫.m...."
                           , "▪.▪.▪.▪.▪.▪"
                           ]
                 ) (Just North) @?= Nothing

    , testCase "really, don't bomb air" $
      shouldBomb ( mkBoard [ "▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪"
                           , "▪▫..m....▫▫..▫▫.▫▫▫▫▫▫..........▪"
                           , "▪.▪.▪.▪.▪.▪.▪.▪.▪.▪▫▪.▪.▪.▪.▪.▪.▪"
                           , "▪.▫▫.......▫..▫.................▪"
                           , "▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪"
                           , "▪..▫........▫▫▫.................▪"
                           , "▪.▪.▪.▪.▪.▪.▪b▪▫▪▫▪.▪0▪.▪.▪.▪.▪.▪"
                           , "▪...▫▫..▫......▫▫▫...0..........▪"
                           , "▪.▪.▪▫▪.▪.▪.▪▫▪▫▪b▪.▪0▪@▪.▪.▪.▪.▪"
                           , "▪......▫...▫..▫.▫▫0000000.......▪"
                           , "▪.▪.▪.▪▫▪.▪.▪.▪▫▪▫▪.▪0▪.▪.▪.▪.▪.▪"
                           , "▪......▫.b....▫.▫.▫..0..........▪"
                           , "▪.▪.▪.▪.▪.▪.▪▫▪.▪.▪▫▪0▪.▪.▪.▪.▪.▪"
                           , "▪...▫....▫.▫.................b..▪"
                           , "▪.▪.▪.▪.▪.▪.▪.▪.▪.▪b▪.▪.▪.▪.▪.▪.▪"
                           , "▪.....▫.........▫b▫▫....▫.......▪"
                           , "▪.▪.▪.▪.▪.▪b▪▫▪.▪.▪▫▪.▪.▪.▪.▪▫▪.▪"
                           , "▪....m.........m.▫▫.....▫......▫▪"
                           , "▪▫▪.▪▫▪.▪.▪.▪.▪.▪b▪.▪.▪.▪.▪.▪.▪.▪"
                           , "▪▫..▫............▫.....▫......▫.▪"
                           , "▪.▪▫▪▫▪.▪.▪.▪.▪.▪.▪.▪▫▪▫▪.▪.▪.▪.▪"
                           , "▪...................▫▫m..▫......▪"
                           , "▪▫▪.▪.▪▫▪.▪.▪.▪.▪▫▪.▪.▪.▪.▪.▪.▪.▪"
                           , "▪...▫.........▫.▫▫...▫b.▫......▫▪"
                           , "▪.▪.▪b▪.▪.▪.▪.▪.▪▫▪.▪.▪.▪.▪.▪.▪.▪"
                           , "▪..▫.▫▫▫...........▫....▫.......▪"
                           , "▪.▪.▪.▪.▪.▪▫▪.▪.▪.▪.▪.▪.▪m▪.▪.▪.▪"
                           , "▪.....▫.....▫.▫....▫.....▫......▪"
                           , "▪▫▪▫▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪"
                           , "▪▫▫..m...m...m............▫.....▪"
                           , "▪.▪.▪.▪▫▪.▪m▪.▪▫▪.▪▫▪.▪.▪.▪.▪.▪.▪"
                           , "▪...▫.....▫..............m..▫...▪"
                           , "▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪"
                           ]
                 ) (Just North) @?= Nothing
    ]

  , testGroup "path finding"
    [ testCase "shortest must not fall into endless loop" $
      shortest isSafe isEmpty ( mkBoard [ "▪▪▪▪▪▪"
                                        , "▪▪▪5▪▪"
                                        , "▪@..▪▪"
                                        , "▪.5.▪▪"
                                        , "▪...5▪"
                                        , "▪▪▪▪▪▪"
                                        ]
                              ) @?= Nothing

    , testCase "shortest must be shortest path" $
      shortest isSafe isEmpty ( mkBoard [ "▪▪▪▪▪▪"
                                        , "▪@..▪▪"
                                        , "5...5▪"
                                        , "▪▪5▪▪▪"
                                        ]
                              ) @?= Just [East, East]

    , testCase "shortest must be really shortest path" $
      shortest (hasTargets .& isSafe) isEmpty
      ( mkBoard [ "▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪"
                , "▪.....m............▫....m.b.....▪"
                , "▪.▪▫▪.▪.▪.▪▫▪.▪.▪.▪.▪.▪.▪.▪.▪.▪▫▪"
                , "▪▫b.....▫..........▫............▪"
                , "▪.▪.▪.▪▫▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪"
                , "▪▫.▫.....▫......▫▫..............▪"
                , "▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪"
                , "▪▫▫......m.....▫b...............▪"
                , "▪▫▪.▪.▪.▪.▪.▪.▪.▪▫▪.▪.▪.▪.▪.▪.▪.▪"
                , "▪.............▫.................▪"
                , "▪.▪.▪.▪.▪.▪.▪.▪.▪p▪.▪.▪.▪.▪.▪.▪.▪"
                , "▪.......b..▫....................▪"
                , "▪.▪▫▪.▪▫▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪"
                , "▪▫▫.▫▫b▫.▫..m...................▪"
                , "▪.▪.▪▫▪.▪▫▪.▪.▪▫▪.▪.▪.▪.▪.▪.▪@▪.▪"
                , "▪▫.▫....▫▫................▫.1.▫.▪"
                , "▪▫▪▫▪▫▪▫▪▫▪▫▪.▪▫▪.▪.▪.▪▫▪.▪.▪.▪.▪"
                , "▪▫▫.▫...........▫........▫..▫.▫.▪"
                , "▪.▪▫▪▫▪.▪.▪.▪▫▪.▪.▪.▪.▪.▪.▪.▪.▪.▪"
                , "▪.....................▫....▫m..m▪"
                , "▪▫▪.▪▫▪.▪.▪.▪b▪.▪.▪.▪.▪.▪.▪.▪.▪.▪"
                , "▪.▫b▫...................▫.......▪"
                , "▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪.▪▫▪.▪.▪m▪"
                , "▪...▫.▫.▫......................▫▪"
                , "▪.▪.▪.▪.▪▫▪.▪.▪.▪.▪.▪.▪.▪▫▪.▪▫▪.▪"
                , "▪....▫..▫▫.▫....................▪"
                , "▪.▪.▪.▪.▪.▪.▪b▪.▪.▪▫▪.▪.▪.▪.▪.▪.▪"
                , "▪.▫▫▫.b...........▫.......▫....▫▪"
                , "▪.▪.▪▫▪.▪▫▪.▪▫▪.▪.▪▫▪.▪.▪.▪m▪.▪.▪"
                , "▪....▫.▫▫..▫.........b......▫...▪"
                , "▪.▪.▪▫▪.▪.▪.▪.▪.▪.▪▫▪.▪.▪.▪.▪.▪.▪"
                , "▪▫▫▫m.▫▫▫▫▫.▫..▫▫.▫▫▫3..▫.▫.....▪"
                , "▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪▪"
                ]
      ) @?= Just [South, South, South]
    ]

  ]

--------------------------------------------------------------------------------

mkBoard' :: [String] -> Board Cell
mkBoard' xs = parseBoard w (T.pack . mconcat $ xs)
  where w = L.length . L.head $ xs

mkBoard :: [String] -> Board MarkedCell
mkBoard = narrow 5 . extend mark . mkBoard'

--------------------------------------------------------------------------------
