{-# LANGUAGE OverloadedStrings #-}
module EjStand
  ( defaultLanguage
  , getVersion
  )
where

import           Data.String                    ( IsString
                                                , fromString
                                                )
import           Data.Version                   ( showVersion )
import           Paths_ejstand                  ( version )
import           Text.Shakespeare.I18N          ( Lang )

getVersion :: IsString a => a
getVersion = fromString . showVersion $ version

defaultLanguage :: Lang
defaultLanguage = "en"
