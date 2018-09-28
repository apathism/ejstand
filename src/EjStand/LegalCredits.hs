{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module EjStand.LegalCredits
  ( renderLegalCredits
  )
where

import           Data.ByteString               (ByteString)
import           Data.FileEmbed                (embedDir)
import           Data.List                     (sortOn)
import           Data.Maybe                    (catMaybes)
import           Data.Text                     (Text, pack)
import qualified Data.Text                     as Text
import           Data.Text.Encoding            (decodeUtf8)
import qualified Data.Text.Lazy                as LT
import           EjStand.StandingModels        (GlobalConfiguration (..))
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet                   (shamletFile)

data CabalPackage = CabalPackage { packageName        :: !Text
                                 , packageLicenseText :: !Text
                                 } deriving (Show)

getCabalPackages :: [CabalPackage]
getCabalPackages = sortOn packageName . catMaybes $ toCabalPackage <$> $(embedDir "third-party/licenses")
 where
  toCabalPackage :: (String, ByteString) -> Maybe CabalPackage
  toCabalPackage (fileName, contents) = case Text.stripSuffix ".txt" $ pack fileName of
    (Just name) -> Just $ CabalPackage name (decodeUtf8 contents)
    _           -> Nothing

renderLegalCredits :: GlobalConfiguration -> LT.Text
renderLegalCredits GlobalConfiguration {..} = renderHtml ($(shamletFile "templates/credits.hamlet"))
