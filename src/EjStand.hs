module EjStand
  ( getVersion
  )
where

import           Data.String   (IsString, fromString)
import           Data.Version  (showVersion)
import           Paths_ejstand (version)

getVersion :: IsString a => a
getVersion = fromString . showVersion $ version
