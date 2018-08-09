{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module EjStand.HtmlRenderer
  ( renderStanding
  )
where

import qualified Data.Map.Strict               as Map
import           Data.Ratio                    (Ratio, denominator, numerator)
import qualified Data.Set                      as Set
import           Data.Text                     (splitOn)
import           Data.Text.Lazy                (Text)
import           Data.Time                     (UTCTime, defaultTimeLocale)
import           Data.Time.Format              (formatTime)
import           EjStand.BaseModels
import           EjStand.StandingBuilder       (takeFromSetBy)
import           EjStand.StandingModels
import           Text.Blaze.Html               (Markup, ToMarkup, preEscapedToMarkup, toMarkup)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              (sub, sup)
import           Text.Hamlet                   (shamletFile)

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [1 ..]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

instance (ToMarkup a, Integral a) => ToMarkup (Ratio a) where
  toMarkup x = let (a, b) = (numerator x, denominator x)
                   aDiv = a `div` b
                   aMod = a `mod` b
               in do
                toMarkup aDiv
                if aMod /= 0 then do
                  sup (toMarkup aMod)
                  preEscapedToMarkup ("&frasl;" :: Text)
                  sub (toMarkup b)
                else
                  ""

instance ToMarkup UTCTime where
  toMarkup = toMarkup . formatTime defaultTimeLocale "%T %d.%m.%y"

getRowCellByProblem :: StandingRow -> Problem -> (Problem, StandingCell)
getRowCellByProblem row@StandingRow {..} prob@Problem {..} = case Map.lookup (problemContest, problemID) rowCells of
  (Just cell) -> (prob, cell)
  Nothing     -> error $ "Can't find standing cell for " ++ show prob ++ " in " ++ show row

renderStanding :: Standing -> Text
renderStanding Standing {..} = renderHtml ($(shamletFile "hamlet/main.hamlet"))
