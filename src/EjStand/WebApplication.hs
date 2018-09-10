{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module EjStand.WebApplication
  ( ejStand
  )
where

import           Control.Exception        (Exception, SomeException, catch, throw)
import           Data.Binary.Builder      (fromByteString)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BSC8
import           Data.String              (IsString, fromString)
import           Data.Text                (Text, pack, unpack)
import           Data.Text.Encoding       (encodeUtf8)
import           Data.Text.Lazy           (toStrict)
import qualified Data.Text.Lazy.Encoding  as EncLazy (encodeUtf8)
import           EjStand.ConfigParser     (retrieveGlobalConfiguration, retrieveStandingConfigs)
import           EjStand.HtmlRenderer     (renderCSS, renderStanding)
import           EjStand.InternalsCore    (textReplaceLast)
import           EjStand.LegalCredits     (renderLegalCredits)
import           EjStand.StandingBuilder  (buildStanding, prepareStandingSource)
import           EjStand.StandingModels
import           Network.HTTP.Types       (ResponseHeaders, Status, status200, status404, status500)
import           Network.Wai              (Application, Request, Response, ResponseReceived, rawPathInfo,
                                           responseBuilder, responseLBS)
import           Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import           System.Clock             (Clock (..), TimeSpec, getTime, nsec, sec)

-- IO Utilities (especially for error handling)

catch' :: Exception e => (e -> IO a) -> IO a -> IO a
catch' = flip catch

catchSomeException' :: (SomeException -> IO a) -> IO a -> IO a
catchSomeException' = catch'

buildExceptionTextMessage :: (IsString s, Monoid s, Exception e) => e -> s
buildExceptionTextMessage exception =
  "500: Internal server error\n\n"
    <> "EjStand got exception during standing table building.\n"
    <> "Please show this log to your server administrator.\n\n"
    <> "Exception details:\n"
    <> fromString (show exception)

buildNotFoundTextMessage :: (IsString s, Monoid s) => Request -> s
buildNotFoundTextMessage _ = "404: Not found\n\nUnable to find a standing corresponding to your URL."

responseBS :: Status -> ResponseHeaders -> ByteString -> Response
responseBS status headers = responseBuilder status headers . fromByteString

onExceptionRespond :: Exception e => (Response -> IO ResponseReceived) -> e -> IO ResponseReceived
onExceptionRespond respond =
  respond . responseBS status500 [("Content-Type", "text/plain")] . buildExceptionTextMessage

-- Routing for URLs

data RoutingException = DuplicateRoutes !ByteString

instance Show RoutingException where
    show (DuplicateRoutes text) = "Multiple routes for URL " ++ BSC8.unpack text

instance Exception RoutingException

isPathCorresponding :: ByteString -> StandingConfig -> Bool
isPathCorresponding path StandingConfig {..} = encodeUtf8 internalName == path -- TODO: this is obviously not enough

-- Main EjStand WAI

runRoute :: GlobalConfiguration -> StandingConfig -> IO Text
runRoute global local = do
  source <- prepareStandingSource global local
  let standing = buildStanding local source
  return . toStrict $ renderStanding global standing

timeSpecToMilliseconds :: TimeSpec -> Integer
timeSpecToMilliseconds time = sum $ [(* 1000) . toInteger . sec, (`div` 1000000) . toInteger . nsec] <*> [time]

insertPageGenerationTime :: Integer -> Text -> Text
insertPageGenerationTime time = textReplaceLast "%%GENERATION_TIME%%" timeText where timeText = pack $ show time

runEjStandRequest :: GlobalConfiguration -> Application
runEjStandRequest global request respond = catchSomeException' (onExceptionRespond respond) $ do
  !startTime <- getTime Monotonic
  local      <- retrieveStandingConfigs global
  let path           = rawPathInfo request
      possibleRoutes = filter (isPathCorresponding path) local
  case (path, possibleRoutes) of
    ("/credits.html", _) ->
      respond $ responseLBS status200 [("Content-Type", "text/html")] $ EncLazy.encodeUtf8 $ renderLegalCredits global
    ("/ejstand.css", _) ->
      respond $ responseLBS status200 [("Content-Type", "text/css")] $ EncLazy.encodeUtf8 renderCSS
    (_, []     ) -> respond $ responseBS status404 [("Content-Type", "text/plain")] $ buildNotFoundTextMessage request
    (_, [route]) -> do
      !pageContents <- runRoute global route
      !finishTime   <- getTime Monotonic
      let !pageGenerationTime = timeSpecToMilliseconds finishTime - timeSpecToMilliseconds startTime
      respond
        $ responseBS status200 [("Content-Type", "text/html")]
        $ encodeUtf8
        $ insertPageGenerationTime pageGenerationTime
        $ pageContents
    _ -> throw $ DuplicateRoutes path

ejStand :: IO ()
ejStand = do
  global@GlobalConfiguration {..} <- retrieveGlobalConfiguration
  let settings = setHost (fromString . unpack $ ejStandHostname) $ setPort ejStandPort $ defaultSettings
  runSettings settings $ runEjStandRequest global
