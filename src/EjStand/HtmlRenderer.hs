{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module EjStand.HtmlRenderer
  ( renderStanding
  , renderCSS
  , placeColumn
  , contestantNameColumn
  , totalScoreColumn
  , totalSuccessesColumn
  , lastSuccessTimeColumn
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
import           EjStand.StandingModels
import           Text.Blaze.Html               (Markup, ToMarkup, preEscapedToMarkup, toMarkup)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              hiding (div)
import           Text.Blaze.Html5.Attributes
import           Text.Hamlet                   (shamletFile)
import           Text.Lucius                   (luciusFile, renderCss)

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
  toMarkup = toMarkup . formatTime defaultTimeLocale "%d.%m.%y %R"

placeColumn :: StandingColumn
placeColumn = StandingColumn caption value
 where
  caption = th ! class_ "place" ! rowspan "2" $ "Место"
  value (place, _) = td ! class_ "place" $ toMarkup place

contestantNameColumn :: StandingColumn
contestantNameColumn = StandingColumn caption value
 where
  caption = th ! class_ "contestant" ! rowspan "2" $ "Имя"
  value (_, row) = td ! class_ "contestant" $ toMarkup . contestantName . rowContestant $ row

totalSuccessesColumn :: StandingColumn
totalSuccessesColumn = StandingColumn caption value
 where
  caption = th ! class_ "total_successes" ! rowspan "2" $ "="
  value (_, row) = td ! class_ "total_successes" $ toMarkup . rowSuccesses . rowStats $ row

totalScoreColumn :: StandingColumn
totalScoreColumn = StandingColumn caption value
 where
  caption = th ! class_ "total_score" ! rowspan "2" $ preEscapedToMarkup ("&Sigma;" :: Text)
  value (_, row) = td ! class_ "total_score" $ toMarkup . rowScore . rowStats $ row

lastSuccessTimeColumn :: StandingColumn
lastSuccessTimeColumn = StandingColumn caption value
 where
  caption = th ! class_ "last_success_time" ! rowspan "2" $ "Время"
  value (_, row) = td ! class_ "last_success_time" $ case rowLastTimeSuccess $ rowStats row of
    Nothing   -> ""
    Just time -> toMarkup time

getRowCellByProblem :: StandingRow -> Problem -> (Problem, StandingCell)
getRowCellByProblem row@StandingRow {..} prob@Problem {..} = case Map.lookup (problemContest, problemID) rowCells of
  (Just cell) -> (prob, cell)
  Nothing     -> error $ "Can't find standing cell for " ++ show prob ++ " in " ++ show row

renderStanding :: Standing -> Text
renderStanding Standing {..} = renderHtml ($(shamletFile "shakespeare/main.hamlet"))

renderCSS :: Text
renderCSS = renderCss ($(luciusFile "shakespeare/main.lucius") undefined)
