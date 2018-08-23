{-# LANGUAGE OverloadedStrings #-}
module EjStand
  ( getVersion
  )
where

import           Data.String (IsString)

getVersion :: IsString a => a
getVersion = "0.2.0"
