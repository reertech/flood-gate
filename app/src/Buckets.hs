{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric #-}

module Buckets ( LimitConfig(..)
               , BucketsInstance
               , summonBuckets
               , inspectBucketInstance
               , claimToken
               , currentBucketStatus
               , BucketStatus(..)
               , lcePeriodSeconds
               , lceRequestsLimit
               ) where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Data.Time.Clock.POSIX (getPOSIXTime)
import Dhall (FromDhall, Natural)
import GHC.Generics

data BucketsInstance = BucketsInstance (TVar [Bucket])

data BucketStatus = BucketStatus
  { bucketStatusNow :: Integer
  , bucketStatusLimitConfigExt :: LimitConfigExt
  } deriving (Show, Eq)

data Bucket = Bucket
  { bucketNow :: Double
  , bucketLastUpdateMicroSec :: Integer
  , bucketLimitConfigExt :: LimitConfigExt
  } deriving (Show, Eq)

data LimitConfig = LimitConfig
  { lcPeriodSeconds :: Natural
  , lcRequestsLimit :: Natural
  } deriving (Generic, Show, Eq)

data LimitConfigExt = LimitConfigExt
  { lcePeriodSeconds :: Integer
  , lceRequestsLimit :: Integer
  , lceIntervalMicroSec :: Double
  } deriving (Show, Eq)

instance FromDhall LimitConfig

getPosixTimeUsecs :: IO Integer
getPosixTimeUsecs = fmap (floor . (*1e6)) getPOSIXTime

inspectBucketInstance :: BucketsInstance -> IO ()
inspectBucketInstance (BucketsInstance tv) = readTVarIO tv >>= print

replentishBucket :: Integer -> Bucket -> Bucket
replentishBucket now bucket =
  let timespan = now - bucketLastUpdateMicroSec bucket
      lce = bucketLimitConfigExt bucket
      deltaTokens = (fromIntegral timespan) / (lceIntervalMicroSec lce)
      newBucketNow = min (fromIntegral $ lceRequestsLimit lce)
                         (bucketNow bucket + deltaTokens)
   in bucket { bucketNow = newBucketNow
             , bucketLastUpdateMicroSec = now
             }

replentishBuckets :: TVar [Bucket] -> IO ()
replentishBuckets tvBuckets = do
  now <- getPosixTimeUsecs
  spinDelay <- atomically $ do
    buckets <- readTVar tvBuckets
    let newBuckets = map (replentishBucket now) buckets
    when (buckets /= newBuckets) $ writeTVar tvBuckets newBuckets
    return $ minimum $ map (lceIntervalMicroSec . bucketLimitConfigExt) newBuckets
  threadDelay $ floor spinDelay

extendLimitConfig :: LimitConfig -> LimitConfigExt
extendLimitConfig lc =
  LimitConfigExt { lceRequestsLimit = limit
                 , lcePeriodSeconds = fromIntegral $ lcPeriodSeconds lc
                 , lceIntervalMicroSec = (fromIntegral $ lcPeriodSeconds lc) * 1000000.0 / limitD
                 }
  where limit = fromIntegral $ lcRequestsLimit lc
        limitD = fromIntegral limit

buildBucket :: Integer -> LimitConfig -> Bucket
buildBucket now lc =
  Bucket { bucketNow = fromIntegral $ lcRequestsLimit lc
         , bucketLastUpdateMicroSec = now
         , bucketLimitConfigExt = extendLimitConfig lc
         }

hasSpareToken :: Bucket -> Bool
hasSpareToken b = bucketNow b >= 1.0

spendToken :: Bucket -> Bucket
spendToken b = b { bucketNow = bucketNow b - 1.0 }

claimToken :: BucketsInstance -> IO ()
claimToken bi@(BucketsInstance tvBuckets) = do
  maybeSpinDelay <- atomically $ do
    buckets <- readTVar tvBuckets
    if (all hasSpareToken buckets)
      then do writeTVar tvBuckets (map spendToken buckets)
              return Nothing
      else return $ Just $ minimum $ map (lceIntervalMicroSec . bucketLimitConfigExt) buckets
  case maybeSpinDelay of
    Nothing -> return ()
    Just spinDelay -> do
      threadDelay (floor spinDelay)
      claimToken bi

mkBucketStatus :: Bucket -> BucketStatus
mkBucketStatus b =
  BucketStatus { bucketStatusNow = floor $ bucketNow b
               , bucketStatusLimitConfigExt = bucketLimitConfigExt b
               }

currentBucketStatus :: BucketsInstance -> IO [BucketStatus]
currentBucketStatus (BucketsInstance tvBuckets) =
  map mkBucketStatus <$> readTVarIO tvBuckets

summonBuckets :: [LimitConfig] -> IO BucketsInstance
summonBuckets limits = do
  now <- getPosixTimeUsecs
  let buckets = map (buildBucket now) limits
  tvBuckets <- newTVarIO buckets
  void $ forkIO $ forever $ replentishBuckets tvBuckets
  return $ BucketsInstance tvBuckets
