{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}

module Main where

import Buckets
import Control.Concurrent
import Control.Monad.Reader
import Data.List
import Dhall (input, auto, FromDhall, Generic)
import Lib
import Prelude hiding (log)
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data ServiceConfig =
  ServiceConfig { proxyConfig :: Config
                , limits :: [LimitConfig]
                } deriving (Generic, Show, Eq)

instance FromDhall ServiceConfig

loadConfig :: T.Text -> IO ServiceConfig
loadConfig dhall = input auto dhall

formatBucketStatus :: BucketStatus -> T.Text
formatBucketStatus BucketStatus { bucketStatusNow = now, bucketStatusLimitConfigExt = lce } =
  (T.pack $ show $ lceRequestsLimit lce)
  <> " rqs per "
  <> formatTimePeriod (lcePeriodSeconds lce)
  <> ": "
  <> formatPercent (now * 100 `div` (lceRequestsLimit lce))
  <> " ("
  <> (T.pack $ show now)
  <> ") avail"
    where formatPercent v = (T.pack $ show v) <> "%"
          formatTimePeriod' _ 0 = ""
          formatTimePeriod' [] secs = error $ show secs <> " secs left when formatting time period"
          formatTimePeriod' ((t, int) : xs) secs =
            let q = secs `quot` int
                r = secs `rem` int
                formatted = if q > 0
                               then (T.pack $ show q) <> t
                               else ""
             in formatted <> formatTimePeriod' xs r
          formatTimePeriod = formatTimePeriod' [("d", 60*60*24), ("h", 60*60), ("m", 60), ("s", 1)]

reportBucketStatus :: (MonadReader Env m, MonadIO m) => BucketsInstance -> m ()
reportBucketStatus bi = forever $ do
  bs <- liftIO $ currentBucketStatus bi
  let formattedBucketStatus = T.intercalate ", " $ map formatBucketStatus bs
  log formattedBucketStatus
  liftIO $ threadDelay 5000000

main' :: [String] -> IO ()
main' [dhall] = do
  svcConfig <- loadConfig $ T.pack dhall
  bi <- summonBuckets $ limits svcConfig
  let env = Env (proxyConfig svcConfig)
                TIO.putStrLn
                (claimToken bi)
  let runEnv = flip runReaderT env
  void $ forkIO $ runEnv $ reportBucketStatus bi
  runEnv serveRateLimitingProxy

main' _ = do
  putStrLn "Please, specify a Dhall expression (e.g. ./config.dhall) to config the service"

main :: IO ()
main = getArgs >>= main'
