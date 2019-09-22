--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import           Bomberman
import           Data.Board
import           Util.Measurement

--------------------------------------------------------------------------------

import           Network.Socket      (withSocketsDo)
import qualified Network.WebSockets  as WS
import           Options.Applicative
import           RIO
import qualified RIO.Text            as T

--------------------------------------------------------------------------------

main :: IO ()
main = execParser optsParser >>= runClient

--------------------------------------------------------------------------------

fetch :: MonadIO m => WS.Connection -> m (Board Cell)
fetch conn = liftIO $ do
  msg <- WS.receiveData conn
  let board
        = parseBoard 33
        . T.map convertCell
        . fromMaybe msg
        . T.stripPrefix "board="
        $ msg
  pure board

push :: MonadIO m => WS.Connection -> Action -> m ()
push conn = liftIO . WS.sendTextData conn . actionToCommand

pipe :: WS.Connection -> RIO App ()
pipe conn
  =   fetch conn
  >>= (\board -> withRunInIO $ \run -> run $ liftRIO $ runPlayer board)
  >>= push conn

runPlayer :: (MonadIO m, MonadReader r m, HasLogFunc r) => Board Cell -> m Action
runPlayer board = do
  (d, a) <- time $ player board
  logInfo $ "executed in " <> secs d
  pure a

--------------------------------------------------------------------------------

runClient :: Opts -> IO ()
runClient opts = withSocketsDo $ WS.runClient
  (optAddress opts)
  (optPort opts)
  url
  (runApp . forever . pipe)
  where url = mconcat [ "/codenjoy-contest/ws?user="
                      , optUser opts
                      , "&code="
                      , optCode opts
                      ]

--------------------------------------------------------------------------------

newtype App
  = App
  { appLogFunc :: LogFunc
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

runApp :: RIO App a -> IO a
runApp inner = do
  logOptions' <- logOptionsHandle stderr False
  let logOptions = setLogUseTime True $ setLogUseLoc False logOptions'
  withLogFunc logOptions $ \logFunc -> do
    let app = App
          { appLogFunc = logFunc
          }
    runRIO app inner

--------------------------------------------------------------------------------

data Opts
  = Opts
  { optAddress :: !String
  , optPort    :: !Int
  , optUser    :: !String
  , optCode    :: !String
  }

optsParser :: ParserInfo Opts
optsParser = info
  (helper <*> versionOption <*> programOptions)
  (fullDesc <> progDesc "bomberman bot" <>
    header
    "bomberman-bot - enjoy the bot playing bomberman")
  where
    versionOption :: Parser (a -> a)
    versionOption = infoOption "0.0" (long "version" <> help "Show version")

    programOptions :: Parser Opts
    programOptions =
        Opts
        <$> strOption ( long "address"
                        <> metavar "VALUE"
                        <> value "34.67.67.45"
                        <> help "Override default address"
                      )
        <*> option auto ( long "port"
                          <> metavar "NUMBER"
                          <> value 8080
                          <> help "Override default port"
                        )
        <*> strOption ( long "user"
                        <> metavar "VALUE"
                        <> help "Set the user"
                      )
        <*> strOption ( long "code"
                        <> metavar "VALUE"
                        <> help "Set the code"
                      )

--------------------------------------------------------------------------------

actionToCommand :: Action -> Text
actionToCommand (Action m b) = T.intercalate "," $ case b of
  NoBomb         -> [moveCommand m]
  BombBeforeMove -> ["ACT", moveCommand m]
  BombAfterMove  -> [moveCommand m, "ACT"]

moveCommand :: Move -> Text
moveCommand Stay         = "STOP"
moveCommand (Move North) = "UP"
moveCommand (Move East)  = "RIGHT"
moveCommand (Move South) = "DOWN"
moveCommand (Move West)  = "LEFT"

--------------------------------------------------------------------------------

convertCell :: Char -> Char
convertCell '☺' = '@'
convertCell '☻' = '!'
convertCell 'Ѡ' = 'x'
convertCell '♥' = 'b'
convertCell '♠' = 'B'
convertCell '♣' = 'p'
convertCell '5' = '5'
convertCell '4' = '4'
convertCell '3' = '3'
convertCell '2' = '2'
convertCell '1' = '1'
convertCell '҉' = '0'
convertCell '☼' = '▪'
convertCell '#' = '▫'
convertCell 'H' = '·'
convertCell '&' = 'm'
convertCell 'x' = 'M'
convertCell _   = '.'

--------------------------------------------------------------------------------
