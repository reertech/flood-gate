{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}

module Main where

import Buckets
import Conduit as C
import Control.Concurrent
import Control.Monad.Reader
import Data.List
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Dhall (input, auto, FromDhall, Generic)
import Lib
import Prelude hiding (log)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data ServiceConfig =
  ServiceConfig { proxyConfig :: Config
                , limits :: [LimitConfig]
                } deriving (Generic, Show, Eq)

instance FromDhall ServiceConfig

loadConfig :: Text -> IO ServiceConfig
loadConfig dhall = input auto dhall

formatBucketStatus :: BucketStatus -> Text
formatBucketStatus BucketStatus { bucketStatusNow = now, bucketStatusLimitConfigExt = lce } =
  (T.pack $ show $ lceRequestsLimit lce)
  <> " rqs per "
  <> formatTimePeriod (lcePeriodSeconds lce)
  <> ": "
  <> formatPercent (now * 100 `div` (lceRequestsLimit lce))
  <> " ("
  <> (T.pack $ show now)
  <> ") left"
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
reportBucketStatus bi = runConduit 
     $ repeatMC (liftIO $ currentBucketStatus bi)
    .| iterMC (const $ liftIO $ threadDelay 100000)
    .| slidingWindowC 2
    .| filterC (\[old, new] -> old /= new)
    .| mapC last
    .| mapC (T.intercalate ", " . map formatBucketStatus)
    .| iterMC log
    .| iterMC (const $ liftIO $ threadDelay 1000000)
    .| sinkNull

timestampedLog :: Text -> IO ()
timestampedLog t = do
  now <- getCurrentTime
  TIO.putStrLn $ "[" <> (T.pack $ iso8601Show now) <> "] " <> t
  hFlush stdout

main' :: [String] -> IO ()
main' [dhall] = do
  svcConfig <- loadConfig $ T.pack dhall
  bi <- summonBuckets $ limits svcConfig
  let env = Env (proxyConfig svcConfig)
                timestampedLog
                (claimToken bi)
  let runEnv = flip runReaderT env
  void $ forkIO $ runEnv $ reportBucketStatus bi
  runEnv serveRateLimitingProxy

main' _ = do
  putStrLn "Please, specify a Dhall expression (e.g. ./config.dhall) to config the service"

main :: IO ()
main = getArgs >>= main'
