{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module EjStand.Web.Server
  ( ejStandWebServer
  )
where

import           Control.Exception              ( Exception
                                                , SomeException
                                                , handle
                                                , throw
                                                )
import           Data.Binary.Builder            ( fromByteString )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as BSC8
import           Data.String                    ( IsString
                                                , fromString
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.Text.Lazy                 ( toStrict )
import qualified Data.Text.Lazy.Encoding       as EncLazy
                                                ( encodeUtf8 )
import           EjStand                        ( defaultLanguage )
import           EjStand.Internals.Core         ( textReplaceLast )
import           EjStand.Models.Standing
import           EjStand.Parsers.Configuration  ( retrieveStandingConfigs )
import           EjStand.StandingBuilder        ( buildStanding
                                                , prepareStandingSource
                                                )
import           EjStand.Web.LegalPage          ( renderLegalCredits )
import           EjStand.Web.MainPage           ( renderCSS
                                                , renderStanding
                                                )
import           Network.HTTP.Types             ( Header
                                                , ResponseHeaders
                                                , Status
                                                , status200
                                                , status404
                                                , status500
                                                )
import           Network.Wai                    ( Application
                                                , Request
                                                , Response
                                                , ResponseReceived
                                                , rawPathInfo
                                                , requestHeaders
                                                , responseBuilder
                                                , responseLBS
                                                )
import           Network.Wai.Handler.Warp       ( defaultSettings
                                                , runSettings
                                                , setHost
                                                , setPort
                                                )
import           System.Clock                   ( Clock(..)
                                                , TimeSpec
                                                , getTime
                                                , nsec
                                                , sec
                                                )
import           Text.Shakespeare.I18N          ( Lang )

-- IO Utilities (especially for error handling)

handleSomeException :: (SomeException -> IO a) -> IO a -> IO a
handleSomeException = handle

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

newtype RoutingException = DuplicateRoutes ByteString

instance Show RoutingException where
  show (DuplicateRoutes text) = "Multiple routes for URL " ++ BSC8.unpack text

instance Exception RoutingException

isPathCorresponding :: ByteString -> StandingConfig -> Bool
isPathCorresponding path StandingConfig {..} = encodeUtf8 internalName == path

-- Main EjStand WAI

runRoute :: GlobalConfiguration -> StandingConfig -> [Lang] -> IO Text
runRoute global local lang = do
  source <- prepareStandingSource global local
  let standing = buildStanding lang local source
  return . toStrict $ renderStanding global standing lang

timeSpecToMilliseconds :: TimeSpec -> Integer
timeSpecToMilliseconds time = sum $ [(* 1000) . toInteger . sec, (`div` 1000000) . toInteger . nsec] <*> [time]

insertPageGenerationTime :: Integer -> Text -> Text
insertPageGenerationTime time = textReplaceLast "%%GENERATION_TIME%%" timeText where timeText = pack $ show time

parseRequestLanguages :: Request -> [Lang]
parseRequestLanguages request = mconcat (parseRequestHeader <$> requestHeaders request) <> [defaultLanguage]
 where
  parseRequestHeader :: Header -> [Lang]
  parseRequestHeader ("Accept-Language", contents) = fst . Text.breakOn ";" <$> Text.splitOn "," (decodeUtf8 contents)
  parseRequestHeader _                             = []

runEjStandRequest :: GlobalConfiguration -> Application
runEjStandRequest global request respond = handleSomeException (onExceptionRespond respond) $ do
  !startTime <- getTime Monotonic
  local      <- retrieveStandingConfigs global
  let path           = rawPathInfo request
      possibleRoutes = filter (isPathCorresponding path) local
      lang           = parseRequestLanguages request
  case (path, possibleRoutes) of
    ("/credits.html", _) ->
      respond $ responseLBS status200 [("Content-Type", "text/html")] $ EncLazy.encodeUtf8 $ renderLegalCredits global
                                                                                                                lang
    ("/ejstand.css", _) ->
      respond $ responseLBS status200 [("Content-Type", "text/css")] $ EncLazy.encodeUtf8 renderCSS
    (_, []     ) -> respond $ responseBS status404 [("Content-Type", "text/plain")] $ buildNotFoundTextMessage request
    (_, [route]) -> do
      !pageContents <- runRoute global route lang
      !finishTime   <- getTime Monotonic
      let !pageGenerationTime = timeSpecToMilliseconds finishTime - timeSpecToMilliseconds startTime
      respond $ responseBS status200 [("Content-Type", "text/html")] $ encodeUtf8 $ insertPageGenerationTime
        pageGenerationTime
        pageContents
    _            -> throw $ DuplicateRoutes path

ejStandWebServer :: GlobalConfiguration -> IO ()
ejStandWebServer global@GlobalConfiguration {..} =
  let settings = setHost (fromString . unpack $ ejStandHostname) $ setPort ejStandPort defaultSettings
  in  runSettings settings $ runEjStandRequest global
