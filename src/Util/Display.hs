--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------------------

module Util.Display where

--------------------------------------------------------------------------------

import           Data.Text.IO (putStrLn)
import           RIO

--------------------------------------------------------------------------------

print :: (MonadIO m, Display a) => a -> m ()
print = liftIO . putStrLn . textDisplay

--------------------------------------------------------------------------------

instance Display a => Display (Maybe a) where
  display Nothing  = "Nothing"
  display (Just v) = display v

--------------------------------------------------------------------------------
