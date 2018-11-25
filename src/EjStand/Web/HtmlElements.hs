{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
module EjStand.Web.HtmlElements
  ( EjStandLocaleMessage(..)
  , EjStandRoute(..)
  , translate
  , skipUrlRendering
  , placeColumn
  , contestantNameColumn
  , totalScoreColumn
  , totalSuccessesColumn
  , lastSuccessTimeColumn
  , getColumnByName
  , renderStandingProblemSuccesses
  , renderCell
  )
where

import           Control.Monad                  ( when )
import           Data.Map.Strict                ( (!?) )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( catMaybes )
import           Data.Ratio                     ( Ratio
                                                , denominator
                                                , numerator
                                                , (%)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time                      ( UTCTime
                                                , defaultTimeLocale
                                                )
import           Data.Time.Format               ( formatTime )
import           EjStand                        ( defaultLanguage )
import           EjStand.Internals.Core         ( (==>) )
import           EjStand.Models.Base
import           EjStand.Models.Standing
import           Prelude                 hiding ( div
                                                , span
                                                )
import qualified Prelude                        ( div )
import           Text.Blaze.Internal            ( Attributable )
import           Text.Blaze.Html                ( Markup
                                                , toMarkup
                                                )
import           Text.Blaze.Html5        hiding ( style
                                                , title
                                                , (!?)
                                                )
import           Text.Blaze.Html5.Attributes
                                         hiding ( span )
import           Text.Hamlet                    ( Render )
import           Text.Shakespeare.I18N

-- Internationalization

data EjStandLocale = EjStandLocale

mkMessage "EjStandLocale" "locale" defaultLanguage

translate :: [Lang] -> EjStandLocaleMessage -> Markup
translate lang = preEscapedText . renderMessage EjStandLocale lang

-- Useless stub for routing: EjStand handles routing by itself

data EjStandRoute = EjStandRoute

skipUrlRendering :: Render EjStandRoute
skipUrlRendering _ _ = "/"

-- Non-standart types rendering

instance (ToMarkup a, Integral a) => ToMarkup (Ratio a) where
    toMarkup x = let (a, b) = (numerator x, denominator x)
                     aDiv = a `Prelude.div` b
                     aMod = a `mod` b
                 in
                  if aMod /= 0 then do
                    when (aDiv /= 0) (toMarkup aDiv)
                    sup (toMarkup aMod)
                    preEscapedText "&frasl;"
                    sub (toMarkup b)
                  else
                    toMarkup aDiv

instance ToMarkup UTCTime where
    toMarkup = toMarkup . formatTime defaultTimeLocale "%d.%m.%y %R"

-- Columns rendering

calculateConditionalStyle :: Attributable h => [ConditionalStyle] -> Rational -> h -> h
calculateConditionalStyle [] _ html = html
calculateConditionalStyle (ConditionalStyle {..} : tail) value html
  | checkComparison value `all` conditions = html ! style (toValue styleValue)
  | otherwise                              = calculateConditionalStyle tail value html

buildCustomDisplayedStandingColumn
  :: Ord a => Text -> Markup -> (Maybe Integer -> StandingRow -> a) -> (a -> Markup) -> StandingColumn
buildCustomDisplayedStandingColumn className caption getter displayF = StandingColumn caption' markupValue order
 where
  caption' = th ! class_ (preEscapedToValue className) ! rowspan "2" $ caption
  markupValue place row = (displayF $ getter (Just place) row) ! class_ (preEscapedToValue className)
  order row1 row2 = getter Nothing row1 `compare` getter Nothing row2

buildRegularStandingColumn
  :: (ToMarkup a, Ord a) => Text -> Markup -> (Maybe Integer -> StandingRow -> a) -> StandingColumn
buildRegularStandingColumn cN cap getter = buildCustomDisplayedStandingColumn cN cap getter displayF
  where displayF value = td $ toMarkup value

placeColumn :: [Lang] -> StandingColumn
placeColumn lang = StandingColumn caption markupValue order
 where
  caption = th ! class_ "place" ! rowspan "2" $ toMarkup $ translate lang MsgPlace
  markupValue place _ = td ! class_ "place" $ toMarkup place
  order _ _ = EQ

contestantNameColumn :: [Lang] -> StandingColumn
contestantNameColumn lang = buildRegularStandingColumn "contestant" caption getter
 where
  caption = toMarkup $ translate lang MsgContestant
  getter _ = contestantName . rowContestant

totalSuccessesColumn :: StandingColumn
totalSuccessesColumn = buildRegularStandingColumn "total_successes" caption getter
 where
  caption = "="
  getter _ = rowSuccesses . rowStats

totalScoreColumn :: StandingConfig -> StandingSource -> StandingColumn
totalScoreColumn StandingConfig {..} StandingSource {..} =
  let caption = preEscapedText "&Sigma;"
      getter _ = rowScore . rowStats
      maxScore = if enableScores then sum $ problemMaxScore <$> problems else toInteger $ Map.size problems
      displayer score = calculateConditionalStyle conditionalStyles (score / (maxScore % 1)) td $ toMarkup score
  in  buildCustomDisplayedStandingColumn "total_score" caption getter displayer

lastSuccessTimeColumn :: [Lang] -> StandingColumn
lastSuccessTimeColumn lang = buildCustomDisplayedStandingColumn "last_success_time" caption getter displayF
 where
  caption = toMarkup $ translate lang MsgLastSuccessTime
  getter _ row = rowLastTimeSuccess $ rowStats row
  displayF Nothing     = td ""
  displayF (Just time) = td $ toMarkup time

getColumnByName :: [Lang] -> StandingConfig -> StandingSource -> Text -> Maybe StandingColumn
getColumnByName lang cfg src columnName = case columnName of
  "Place"       -> Just $ placeColumn lang
  "Name"        -> Just $ contestantNameColumn lang
  "Successes"   -> Just $ totalSuccessesColumn
  "Score"       -> Just $ totalScoreColumn cfg src
  "LastSuccess" -> Just $ lastSuccessTimeColumn lang
  _             -> Nothing

-- Cell rendering

type CellContentBuilder = StandingCell -> Markup

scoreCellContent :: CellContentBuilder
scoreCellContent StandingCell {..} = if cellType == Ignore then mempty else span ! class_ "score" $ toMarkup cellScore

wrongAttemptsCellContent :: CellContentBuilder
wrongAttemptsCellContent StandingCell {..} = case cellAttempts of
  0 -> mempty
  _ -> span ! class_ "wrong_attempts" $ toMarkup cellAttempts

attemptsCellContent :: CellContentBuilder
attemptsCellContent StandingCell {..} = if cellType == Ignore
  then mempty
  else span ! class_ "attempts" $ toMarkup count
 where
  count = case cellType of
    Mistake -> cellAttempts
    _       -> cellAttempts + 1

selectAdditionalCellContentBuilders :: Standing -> [CellContentBuilder]
selectAdditionalCellContentBuilders Standing { standingConfig = StandingConfig {..}, ..} = mconcat
  [ enableScores ==> scoreCellContent
  , showAttemptsNumber ==> if enableScores then attemptsCellContent else wrongAttemptsCellContent
  ]

buildCellTitle :: Standing -> StandingRow -> Problem -> StandingCell -> Text
buildCellTitle Standing { standingConfig = StandingConfig {..}, standingSource = StandingSource {..}, ..} StandingRow {..} Problem {..} StandingCell {..}
  = T.intercalate ", " $ mconcat
    [ [contestantName rowContestant, mconcat [problemShortName, " (", problemLongName, ")"]]
    , catMaybes $ showLanguages ==> (languageLongName <$> (cellMainRun >>= runLanguage >>= (languages !?)))
    ]

renderCell :: Standing -> StandingRow -> Problem -> CellContentBuilder
renderCell st@Standing { standingConfig = StandingConfig {..}, ..} row problem cell@StandingCell {..} =
  cellTag' $ foldl (>>) cellValue additionalContent
 where
  additionalContent    = if allowCellContent then selectAdditionalCellContentBuilders st <*> [cell] else []
  addRunStatusCellText = span ! class_ "run_status"
  ifNotScores x = if enableScores then mempty else x
  cellTag'                               = cellTag ! title (toValue $ buildCellTitle st row problem cell)
  (cellTag, cellValue, allowCellContent) = case cellType of
    Success -> if cellIsOverdue
      then (td ! class_ "overdue", ifNotScores $ addRunStatusCellText "+.", True)
      else (td ! class_ "success", ifNotScores $ addRunStatusCellText "+", True)
    Processing   -> (td ! class_ "processing", ifNotScores $ addRunStatusCellText "-", True)
    Pending      -> (td ! class_ "pending", ifNotScores $ addRunStatusCellText "?", True)
    Rejected     -> (td ! class_ "rejected", ifNotScores $ addRunStatusCellText "-", True)
    Mistake      -> (td ! class_ "mistake", ifNotScores $ addRunStatusCellText "-", True)
    Ignore       -> (td ! class_ "none", "", True)
    Disqualified -> (td ! class_ "disqualified", "", False)
    Error        -> (td ! class_ "error", addRunStatusCellText "✖", False)

renderProblemSuccesses :: Standing -> Problem -> Markup
renderProblemSuccesses Standing {..} Problem {..} =
  let countProblemSuccesses =
        length
          .   filter ((== Success) . cellType)
          .   catMaybes
          $   Map.lookup (problemContest, problemID)
          .   rowCells
          <$> standingRows
  in  td ! class_ "problem_successes row_value" $ toMarkup countProblemSuccesses

renderStandingProblemSuccesses :: [Lang] -> Standing -> Markup
renderStandingProblemSuccesses lang standing@Standing {..} =
  let header = td ! class_ "problem_successes row_header" ! colspan (toValue . length $ standingColumns) $ translate
        lang
        MsgCorrectSolutions
  in  tr $ foldl (>>) header $ renderProblemSuccesses standing <$> standingProblems
