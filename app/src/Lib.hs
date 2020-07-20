{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric #-}

module Lib
    ( serveRateLimitingProxy
    , log
    , Env(..)
    , Config(..)
    ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.STM
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Dhall (FromDhall, Generic, Natural)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.ReverseProxy
import Network.HTTP.Types.Status (gatewayTimeout504)
import Network.Socket (SockAddr)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Timeout
import Prelude hiding (log)

import qualified Data.Text as T

data Config = Config
  { configPort :: Natural
  , configTargetHost :: Text
  , configTargetPort :: Natural
  , configIsTargetTls :: Bool
  , configMaxConn :: Natural
  , configServeTimeoutSecs :: Natural
  } deriving (Generic, Show, Eq)

instance FromDhall Config

data Env = Env
  { envConfig :: Config
  , envLog :: !(Text -> IO ())
  , envClaimToken :: IO ()
  }

log :: (MonadReader Env m, MonadIO m) => Text -> m ()
log msg = asks envLog >>= (\f -> liftIO $ f msg)

fixHeaders :: ByteString -> Request -> Request
fixHeaders host rq =
  rq { isSecure = True
     , requestHeaders = ("Host", host) : filter ((/=) "Host" . fst) (requestHeaders rq)
     , requestHeaderHost = Just host
     }

gatewayTimeout :: WaiProxyResponse
gatewayTimeout =
  WPRResponse $ responseLBS gatewayTimeout504 [] "Rate limit induced time out"

forward :: Env -> Request -> IO WaiProxyResponse
forward env rq = do
  let claimToken = envClaimToken env
      logMsg = envLog env
      cfg = envConfig env
      host = encodeUtf8 $ configTargetHost cfg
      port = fromIntegral $ configTargetPort cfg
      serveTimeout = fromIntegral $ configServeTimeoutSecs cfg
      mkProxyResponse = if configIsTargetTls cfg
                          then WPRModifiedRequestSecure
                          else WPRModifiedRequest
  claimed <- race claimToken
                  (threadDelay 100000 >> logMsg "Request getting delayed 100ms+ due to rate limits" >>
                    threadDelay (serveTimeout * 1000000))
  return $ case claimed of
    Left _ -> mkProxyResponse (fixHeaders host rq) (ProxyDest host port)
    Right _ -> gatewayTimeout

buildProxyApp :: (MonadReader Env m, MonadIO m) => m Application
buildProxyApp = do
  manager <- liftIO $ newManager tlsManagerSettings
  env <- ask
  return $ waiProxyTo (forward env) defaultOnExc manager

openHandler :: (Text -> IO ()) -> Int -> TVar Int -> SockAddr -> IO Bool
openHandler logMsg maxConn ref _ = do
  result <- atomically $ do
    connections <- readTVar ref
    if connections < maxConn
       then writeTVar ref (connections + 1) >> return True
       else return False
  when (not result) $ logMsg "Denying connection due to too many being in flight already"
  return result

closeHandler :: TVar Int -> SockAddr -> IO ()
closeHandler ref _ =
  atomically $ do
    connections <- readTVar ref
    writeTVar ref $ max 0 (connections - 1)

serveRateLimitingProxy :: (MonadReader Env m, MonadIO m ) => m ()
serveRateLimitingProxy = do
  connCounter <- liftIO $ newTVarIO 0
  app <- buildProxyApp
  bindPort <- asks (configPort . envConfig)
  maxConn <- asks (configMaxConn . envConfig)
  timeoutSecs <- asks (fromIntegral . configServeTimeoutSecs . envConfig)
  logMsg <- asks envLog
  log $ "Starting rate limiting proxy on :" <> (T.pack $ show bindPort) <> "..."
  let waiSettings = setPort (fromIntegral bindPort)
                  $ setOnOpen (openHandler logMsg (fromIntegral maxConn) connCounter)
                  $ setOnClose (closeHandler connCounter)
                  $ defaultSettings
  liftIO $ runSettings waiSettings $ timeoutStatus gatewayTimeout504 (timeoutSecs + 1) $ app
  log $ "Shutting down the proxy gracefully"
