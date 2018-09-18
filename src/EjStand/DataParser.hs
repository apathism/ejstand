{-# LANGUAGE OverloadedStrings #-}
module EjStand.DataParser
  ( parseEjudgeXML
  , parseEjudgeXMLs
  )
where

import           EjStand.StandingModels (StandingSource (..))

parseEjudgeXML :: FilePath -> IO StandingSource
parseEjudgeXML file = undefined

parseEjudgeXMLs :: [FilePath] -> IO StandingSource
parseEjudgeXMLs filelist = do
  sources <- sequence $ map parseEjudgeXML filelist
  return $ mconcat sources
