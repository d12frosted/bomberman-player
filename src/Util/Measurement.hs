--------------------------------------------------------------------------------

-- | Code execution time measurement.

--------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

module Util.Measurement
  ( time
  , time_
  , secs
  ) where

--------------------------------------------------------------------------------

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.String            (IsString, fromString)
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import           Text.Printf            (printf)

--------------------------------------------------------------------------------

getTime :: MonadIO m => m Double
getTime = liftIO $ realToFrac <$> getPOSIXTime

--------------------------------------------------------------------------------

time :: MonadIO m => m a -> m (Double, a)
time act = do
  start <- getTime
  result <- act
  end <- getTime
  let !delta = end - start
  return (delta, result)

time_ :: MonadIO m => m a -> m Double
time_ act = do
  start <- getTime
  _ <- act
  end <- getTime
  return $! end - start

--------------------------------------------------------------------------------

secs :: (IsString a) => Double -> a
secs = fromString . secs'

secs' :: Double -> String
secs' k
    | k < 0      = '-' : secs' (-k)
    | k >= 1     = k        `with` "s"
    | k >= 1e-3  = (k*1e3)  `with` "ms"
    | k >= 1e-6  = (k*1e6)  `with` "us"
    | k >= 1e-9  = (k*1e9)  `with` "ns"
    | k >= 1e-12 = (k*1e12) `with` "ps"
    | otherwise  = printf "%g s" k
     where with (t :: Double) (u :: String)
               | t >= 1e9  = printf "%.4g %s" t u
               | t >= 1e6  = printf "%.0f %s" t u
               | t >= 1e5  = printf "%.1f %s" t u
               | t >= 1e4  = printf "%.2f %s" t u
               | t >= 1e3  = printf "%.3f %s" t u
               | t >= 1e2  = printf "%.4f %s" t u
               | t >= 1e1  = printf "%.5f %s" t u
               | otherwise = printf "%.6f %s" t u

--------------------------------------------------------------------------------
