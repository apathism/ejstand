{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module EjStand.LegalCredits
  ( renderLegalCredits
  )
where

import           Data.ByteString               (ByteString)
import           Data.FileEmbed                (embedDir)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (catMaybes)
import           Data.String                   (IsString)
import           Data.Text                     (Text, pack)
import           Data.Text.Encoding            (decodeUtf8)
import qualified Data.Text.Lazy                as LT
import           EjStand.StandingModels        (GlobalConfiguration (..))
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet                   (shamletFile)

data CabalPackage = CabalPackage { packageName        :: !Text
                                 , packageLicenseText :: !Text
                                 } deriving (Show)

getDependenciesStringList :: IsString s => [s]
getDependenciesStringList =
  [ "base"
  , "bytestring"
  , "containers"
  , "safe"
  , "text"
  , "clock"
  , "time"
  , "directory"
  , "file-embed"
  , "http-types"
  , "wai"
  , "warp"
  , "blaze-html"
  , "shakespeare"
  , "xml-conduit"
  , "binary"
  , "mtl"
  ]

getDependenciesLicenses :: Map Text ByteString
getDependenciesLicenses = Map.fromList $ (\(a, b) -> (pack a, b)) <$> $(embedDir "third-party/licenses")

getCabalPackages :: [CabalPackage]
getCabalPackages =
  catMaybes
    $   toMaybePackage
    <$> (\dep -> (dep :: Text, Map.lookup (dep <> ".txt") getDependenciesLicenses))
    <$> getDependenciesStringList
 where
  toMaybePackage :: (Text, Maybe ByteString) -> Maybe CabalPackage
  toMaybePackage (name, Nothing       ) = Nothing
  toMaybePackage (name, (Just license)) = Just $ CabalPackage name $ decodeUtf8 license

renderLegalCredits :: GlobalConfiguration -> LT.Text
renderLegalCredits GlobalConfiguration {..} = renderHtml ($(shamletFile "templates/credits.hamlet"))
