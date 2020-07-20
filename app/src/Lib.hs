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
import Control.Exception (onException)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.STM
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Dhall
import GHC.Generics
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.ReverseProxy
import Network.HTTP.Types.Status (gatewayTimeout504)
import Network.Socket (SockAddr)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Timeout
import Prelude hiding (log)

import Control.Exception (SomeException)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Config = Config
  { configPort :: Natural
  , configTargetHost :: Text
  , configTargetPort :: Natural
  , configIsTargetTls :: Bool
  , configMaxConn :: Natural
  } deriving (Generic, Show, Eq)

instance FromDhall Config

data Env = Env
  { envConfig :: Config
  , envLog :: !(T.Text -> IO ())
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

forward :: IO () -> (Request -> ProxyDest -> WaiProxyResponse) -> ByteString -> Int -> Request -> IO WaiProxyResponse
forward claimToken mkProxyResponse host port rq = do
  claimed <- race claimToken
                  (threadDelay 2000000 >> putStrLn "Request getting delayed due to rate limits" >> threadDelay 28000000)
  return $ case claimed of
    Left _ -> mkProxyResponse (fixHeaders host rq) (ProxyDest host port)
    Right _ -> gatewayTimeout

buildProxyApp :: (MonadReader Env m, MonadIO m) => m Application
buildProxyApp = do
  manager <- liftIO $ newManager tlsManagerSettings
  cfg <- asks envConfig
  claimToken <- asks envClaimToken
  let host = encodeUtf8 $ configTargetHost cfg
      port = configTargetPort cfg
      requestBuilder = if configIsTargetTls cfg
                          then WPRModifiedRequestSecure
                          else WPRModifiedRequest
  return $ waiProxyTo (forward claimToken requestBuilder host (fromIntegral port))
                      defaultOnExc
                      manager

{-customOnException :: Maybe Request -> SomeException -> IO ()-}
{-customOnException rq e = do-}
  {-putStrLn ""-}
  {-print rq-}
  {-TIO.putStrLn $ T.pack $ show e-}

openHandler :: Int -> TVar Int -> SockAddr -> IO Bool
openHandler maxConn ref _ = do
  atomically $ do
    connections <- readTVar ref
    if connections < maxConn
       then writeTVar ref (connections + 1) >> return True
       else return False

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
  log $ "Starting rate limiting proxy on :" <> (T.pack $ show bindPort) <> "..."
  let waiSettings = setPort (fromIntegral bindPort)
                  $ setOnOpen (openHandler (fromIntegral maxConn) connCounter)
                  $ setOnClose (closeHandler connCounter)
                  $ defaultSettings
  liftIO $ runSettings waiSettings $ timeoutStatus gatewayTimeout504 60 $ app
  log $ "Shutting down the proxy gracefully"
