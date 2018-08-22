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
import           Data.String                   (IsString)
import           Data.Text                     (splitOn)
import           Data.Text.Lazy                (Text)
import           Data.Time                     (UTCTime, defaultTimeLocale)
import           Data.Time.Format              (formatTime)
import           EjStand.BaseModels
import           EjStand.InternalsCore         (takeFromSetBy, (==>))
import           EjStand.StandingModels
import           Prelude                       hiding (div, span)
import qualified Prelude                       (div)
import           Text.Blaze.Html               (Markup, ToMarkup, preEscapedToMarkup, toMarkup)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes   hiding (span)
import           Text.Hamlet                   (shamletFile)
import           Text.Lucius                   (luciusFile, renderCss)

-- Utilities for templates

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [1 ..]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

getRowCellByProblem :: StandingRow -> Problem -> (Problem, StandingCell)
getRowCellByProblem row@StandingRow {..} prob@Problem {..} = case Map.lookup (problemContest, problemID) rowCells of
  (Just cell) -> (prob, cell)
  Nothing     -> error $ "Can't find standing cell for " ++ show prob ++ " in " ++ show row

-- Non-standart types rendering

instance (ToMarkup a, Integral a) => ToMarkup (Ratio a) where
  toMarkup x = let (a, b) = (numerator x, denominator x)
                   aDiv = a `Prelude.div` b
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

-- Columns rendering

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

-- Cell rendering

type CellContentBuilder = StandingCell -> Markup

scoreCellContent :: CellContentBuilder
scoreCellContent StandingCell {..} = span ! class_ "score" $ toMarkup cellScore

wrongAttemptsCellContent :: CellContentBuilder
wrongAttemptsCellContent StandingCell {..} = case cellAttempts of
  0 -> mempty
  _ -> span ! class_ "wrong_attempts" $ toMarkup cellAttempts

attemptsCellContent :: CellContentBuilder
attemptsCellContent StandingCell {..} = if count == 0 then mempty else span ! class_ "attempts" $ toMarkup count
 where
  count = case cellType of
    Success -> cellAttempts + 1
    Ignore  -> 0
    _       -> cellAttempts

selectAdditionalCellContentBuilders :: StandingConfig -> [CellContentBuilder]
selectAdditionalCellContentBuilders StandingConfig {..} =
  mconcat [ elem EnableScores standingOptions ==> scoreCellContent
          , elem ShowAttemptsNumber standingOptions ==> if elem EnableScores standingOptions then attemptsCellContent else wrongAttemptsCellContent]

renderCell :: StandingConfig -> CellContentBuilder
renderCell cfg@StandingConfig {..} cell@StandingCell {..} = cellTag $ foldl (>>) cellValue additionalContent
 where
  additionalContent = selectAdditionalCellContentBuilders cfg <*> [cell]
  addRunStatusCellText text = span ! class_ "run_status" $ text
  cellValue             = if elem EnableScores standingOptions then mempty else cellValue'
  (cellTag, cellValue') = case cellType of
    Success -> case cellIsOverdue of
      False -> (td ! class_ "success", addRunStatusCellText "+")
      True  -> (td ! class_ "overdue", addRunStatusCellText "+.")
    Pending      -> (td ! class_ "pending", addRunStatusCellText "?")
    Rejected     -> (td ! class_ "rejected", addRunStatusCellText "-")
    Mistake      -> (td ! class_ "mistake", addRunStatusCellText "-")
    Ignore       -> (td ! class_ "none", "")
    Disqualified -> (td ! class_ "disqualified", "")
    Error        -> (td ! class_ "error", "")

-- Main entry points

renderStanding :: Standing -> Text
renderStanding Standing {..} = renderHtml ($(shamletFile "shakespeare/main.hamlet"))

renderCSS :: Text
renderCSS = renderCss ($(luciusFile "shakespeare/main.lucius") undefined)
