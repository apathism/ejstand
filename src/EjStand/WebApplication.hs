{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module EjStand.WebApplication where

import           Control.Exception        (Exception, SomeException, catch)
import           Data.Binary.Builder      (fromByteString)
import           Data.String              (IsString, fromString)
import           Data.Text                (unpack)
import           EjStand.ConfigParser
import           EjStand.StandingModels
import           Network.HTTP.Types       (status200, status500)
import           Network.Wai              (Application, Response, ResponseReceived, rawPathInfo, responseBuilder)
import           Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)

-- IO Utilities (especially for error handling)

emptyIO :: IO ()
emptyIO = return ()

catch' :: Exception e => (e -> IO a) -> IO a -> IO a
catch' = flip catch

catchSomeException' :: (SomeException -> IO a) -> IO a -> IO a
catchSomeException' = catch'

buildExceptionTextMessage :: (IsString s, Exception e) => e -> s
buildExceptionTextMessage exception =
  fromString
    $  "EjStand got exception during standing table building.\n"
    ++ "Please show this log to your server administrator.\n\n"
    ++ "Exception details:\n"
    ++ show exception

onExceptionRespond :: Exception e => (Response -> IO ResponseReceived) -> e -> IO ResponseReceived
onExceptionRespond respond =
  respond . responseBuilder status500 [("Content-Type", "text/plain")] . fromByteString . buildExceptionTextMessage

-- Main EjStand WAI

ejStand :: GlobalConfiguration -> Application
ejStand cfg@GlobalConfiguration {..} request respond = catchSomeException' (onExceptionRespond respond) $ do
    standingConfigs <- retrieveStandingConfigs cfg
    respond $ responseBuilder status200 [("Content-Type", "text/plain")] $ fromByteString $ rawPathInfo request

runEjStand :: IO ()
runEjStand = do
  cfg@GlobalConfiguration {..} <- retrieveGlobalConfiguration
  let settings = setHost (fromString . unpack $ ejStandHostname) $ setPort ejStandPort $ defaultSettings
  runSettings settings $ ejStand cfg
