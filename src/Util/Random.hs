--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------------------------------------

module Util.Random where

--------------------------------------------------------------------------------

import           RIO
import           RIO.List.Partial ((!!))
import           System.Random

--------------------------------------------------------------------------------

randomElement :: MonadIO m => [a] -> m (Maybe a)
randomElement []  = pure Nothing
randomElement [x] = pure . Just $ x
randomElement ls  = do
  idx <- liftIO $ getStdRandom $ randomR (0, length ls - 1)
  pure . Just $ ls !! idx

--------------------------------------------------------------------------------
