{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module EjStand.Web.HtmlElements
  ( EjStandLocaleMessage(..)
  , EjStandRoute(..)
  , PlaceColumn(..)
  , UserIDColumn(..)
  , ContestantNameColumn(..)
  , TotalScoreColumn(..)
  , TotalSuccessesColumn(..)
  , LastSuccessTimeColumn(..)
  , translate
  , skipUrlRendering
  , getColumnByVariant
  , getColumnsByVariantWithStyles
  , renderStandingProblemSuccesses
  , renderCell
  )
where

import           Control.Monad                  ( when )
import           Data.Function                  ( on )
import           Data.Map.Strict                ( (!?) )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( catMaybes )
import           Data.Ratio                     ( Ratio
                                                , denominator
                                                , numerator
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time                      ( NominalDiffTime
                                                , UTCTime
                                                , defaultTimeLocale
                                                , diffUTCTime
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
import           Text.Printf                    ( printf )
import           Text.Shakespeare.I18N

-- Internationalization

data EjStandLocale = EjStandLocale

mkMessage "EjStandLocale" "locale" defaultLanguage

translate :: [Lang] -> EjStandLocaleMessage -> Text
translate = renderMessage EjStandLocale

-- Useless stub for routing: EjStand handles routing by itself

data EjStandRoute = EjStandRoute

skipUrlRendering :: Render EjStandRoute
skipUrlRendering _ _ = "/"

-- Non-standart types rendering

instance (ToMarkup a, Integral a) => ToMarkup (Ratio a) where
  toMarkup x =
    let (a, b) = (numerator x, denominator x)
        aDiv   = a `Prelude.div` b
        aMod   = a `mod` b
    in  if aMod /= 0
          then do
            when (aDiv /= 0) (toMarkup aDiv)
            sup (toMarkup aMod)
            preEscapedText "&frasl;"
            sub (toMarkup b)
          else toMarkup aDiv

instance ToMarkup UTCTime where
  toMarkup = toMarkup . formatTime defaultTimeLocale "%d.%m.%y %R"

-- Columns rendering

newtype PlaceColumn = PlaceColumn { lang :: [Lang] }

instance StandingColumn PlaceColumn Integer where
  columnTagClass = const "place"
  columnCaptionText PlaceColumn {..} = preEscapedText $ translate lang MsgPlace
  columnValue _ = const
  columnOrder _ _ _ = EQ
  columnValueDisplayer _ = toMarkup

newtype UserIDColumn = UserIDColumn { lang :: [Lang] }

instance StandingColumn UserIDColumn Integer where
  columnTagClass = const "user_id"
  columnCaptionText UserIDColumn {..} = preEscapedText $ translate lang MsgUserID
  columnValue _ _ = contestantID . rowContestant
  columnOrder column = compare `on` columnValue column (-1)
  columnValueDisplayer _ = toMarkup

newtype ContestantNameColumn = ContestantNameColumn { lang :: [Lang] }

instance StandingColumn ContestantNameColumn Text where
  columnTagClass = const "contestant"
  columnCaptionText ContestantNameColumn {..} = preEscapedText $ translate lang MsgContestant
  columnValue _ _ = contestantName . rowContestant
  columnOrder column = compare `on` columnValue column (-1)
  columnValueDisplayer _ = toMarkup

newtype TotalSuccessesColumn = TotalSuccessesColumn { lang :: [Lang] }

instance StandingColumn TotalSuccessesColumn Integer where
  columnTagClass = const "total_successes"
  columnCaptionText _ = "="
  columnCaptionTitleText TotalSuccessesColumn {..} = Just $ translate lang MsgSuccessesCaptionTitle
  columnValue _ _ = rowSuccesses . rowStats
  columnOrder column = compare `on` columnValue column (-1)
  columnValueDisplayer _ = toMarkup

newtype TotalAttemptsColumn = TotalAttemptsColumn { lang :: [Lang] }

instance StandingColumn TotalAttemptsColumn Integer where
  columnTagClass = const "total_attempts"
  columnCaptionText _ = "!"
  columnCaptionTitleText TotalAttemptsColumn {..} = Just $ translate lang MsgAttemptsCaptionTitle
  columnValue _ _ = rowAttempts . rowStats
  columnOrder column = compare `on` columnValue column (-1)
  columnValueDisplayer _ = toMarkup

newtype TotalScoreColumn = TotalScoreColumn { lang :: [Lang] }

instance StandingColumn TotalScoreColumn Rational where
  columnTagClass = const "total_score"
  columnCaptionText _ = preEscapedText "&Sigma;"
  columnCaptionTitleText TotalScoreColumn {..} = Just $ translate lang MsgTotalScoreCaptionTitle
  columnValue _ _ = rowScore . rowStats
  columnOrder column = compare `on` columnValue column (-1)
  columnValueDisplayer _ = toMarkup

newtype LastSuccessTimeColumn = LastSuccessTimeColumn { lang :: [Lang] }

instance StandingColumn LastSuccessTimeColumn (Maybe UTCTime) where
  columnTagClass = const "last_success_time"
  columnCaptionText LastSuccessTimeColumn {..} = preEscapedText $ translate lang MsgLastSuccessTime
  columnCaptionTitleText LastSuccessTimeColumn {..} = Just $ translate lang MsgLastSuccessTimeCaptionTitle
  columnValue _ _ = rowLastTimeSuccess . rowStats
  columnOrder column = compare `on` columnValue column (-1)
  columnValueDisplayer _ Nothing     = ""
  columnValueDisplayer _ (Just time) = toMarkup time

getColumnByVariant :: [Lang] -> ColumnVariant -> GenericStandingColumn
getColumnByVariant lang columnV = case columnV of
  PlaceColumnVariant           -> GenericStandingColumn $ PlaceColumn lang
  UserIDColumnVariant          -> GenericStandingColumn $ UserIDColumn lang
  NameColumnVariant            -> GenericStandingColumn $ ContestantNameColumn lang
  SuccessesColumnVariant       -> GenericStandingColumn $ TotalSuccessesColumn lang
  AttemptsColumnVariant        -> GenericStandingColumn $ TotalAttemptsColumn lang
  ScoreColumnVariant           -> GenericStandingColumn $ TotalScoreColumn lang
  LastSuccessTimeColumnVariant -> GenericStandingColumn $ LastSuccessTimeColumn lang

getColumnsByVariantWithStyles
  :: [Lang] -> StandingConfig -> StandingSource -> [ColumnVariant] -> [GenericStandingColumn]
getColumnsByVariantWithStyles lang StandingConfig {..} src columnV = getColumnByVariant lang <$> columnV

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

displayContestTime :: NominalDiffTime -> Markup
displayContestTime time =
  let total_minutes = floor time `Prelude.div` 60 :: Integer
      hours         = total_minutes `Prelude.div` 60
      minutes       = total_minutes `Prelude.mod` 60
  in  toMarkup (printf "%d:%02d" hours minutes :: String)

successTimeCellContent :: CellContentBuilder
successTimeCellContent StandingCell {..} = if cellType /= Success
  then mempty
  else case cellMainRun of
    Nothing       -> mempty
    Just Run {..} -> span ! class_ "success_time" $ displayContestTime $ runTime `diffUTCTime` cellStartTime

selectAdditionalCellContentBuilders :: Standing -> [CellContentBuilder]
selectAdditionalCellContentBuilders Standing { standingConfig = StandingConfig {..}, ..} = mconcat
  [ enableScores ==> scoreCellContent
  , showAttemptsNumber ==> if enableScores then attemptsCellContent else wrongAttemptsCellContent
  , showSuccessTime ==> successTimeCellContent
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
    Success      -> if cellIsOverdue
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
  let header =
        td
          ! class_ "problem_successes row_header"
          ! colspan (toValue . length $ standingColumns)
          $ preEscapedText
          $ translate lang MsgCorrectSolutions
  in  tr $ foldl (>>) header $ renderProblemSuccesses standing <$> standingProblems
