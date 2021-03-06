{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module EjStand.Web.HtmlElements
  ( EjStandLocaleMessage(..)
  , EjStandRoute(..)
  , ConditionalStyleRuntimeException(..)
  , translate
  , skipUrlRendering
  , getColumnByVariant
  , getColumnByVariantWithStyles
  , calculateProblemRating
  , renderStandingProblemSuccesses
  , renderCell
  )
where

import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad                  ( when )
import           Data.Char                     as Char
import           Data.Function                  ( on )
import           Data.Map.Strict                ( Map
                                                , (!?)
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( catMaybes )
import           Data.Ratio                     ( Ratio
                                                , denominator
                                                , numerator
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Time                      ( NominalDiffTime
                                                , UTCTime
                                                , defaultTimeLocale
                                                , diffUTCTime
                                                )
import           Data.Time.Format               ( formatTime )
import           EjStand                        ( defaultLanguage )
import qualified EjStand.ELang                 as ELang
import           EjStand.Internals.Core
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

displayRatio :: FractionDisplayStyle -> Ratio Integer -> Markup
displayRatio style x =
  let (a, b) = (numerator x, denominator x)
  in  case style of
        DisplayAsFraction -> if aMod /= 0
          then do
            when (aDiv /= 0) (toMarkup aDiv)
            sup (toMarkup aMod)
            preEscapedText "&frasl;"
            sub (toMarkup b)
          else toMarkup aDiv
          where (aDiv, aMod) = a `divMod` b
        (DisplayAsDecimal digits) ->
          let newDenominator  = 10 ^ digits
              newNumerator    = (a * newDenominator) `Prelude.div` b
              (before, after) = newNumerator `divMod` newDenominator
          in  do
                toMarkup before
                when (before /= 0) (toMarkup ((printf ".%0*d" digits after) :: String))

instance ToMarkup UTCTime where
  toMarkup = toMarkup . formatTime defaultTimeLocale "%d.%m.%y %R"

-- Columns rendering

newtype PlaceColumn = PlaceColumn { standing :: Standing }

instance StandingColumn PlaceColumn where
  type StandingColumnValue PlaceColumn = Integer
  columnTagClass = const "place"
  columnCaptionText PlaceColumn { standing = Standing {..} } = preEscapedText $ translate standingLanguage MsgPlace
  columnValue _ = const
  columnOrder _ _ _ = EQ
  columnValueDisplayer _ = toMarkup
  columnMaxValue =
    Just $ \PlaceColumn { standing = Standing {..} } -> toInteger . Map.size $ contestants standingSource

newtype UserIDColumn = UserIDColumn { standing :: Standing }

instance StandingColumn UserIDColumn where
  type StandingColumnValue UserIDColumn = Integer
  columnTagClass = const "user_id"
  columnCaptionText UserIDColumn { standing = Standing {..} } = preEscapedText $ translate standingLanguage MsgUserID
  columnValue _ _ = contestantID . rowContestant
  columnOrder column = compare `on` columnValue column (-1)
  columnValueDisplayer _ = toMarkup

newtype ContestantNameColumn = ContestantNameColumn { standing :: Standing }

instance StandingColumn ContestantNameColumn where
  type StandingColumnValue ContestantNameColumn = Text
  columnTagClass = const "contestant"
  columnCaptionText ContestantNameColumn { standing = Standing {..} } =
    preEscapedText $ translate standingLanguage MsgContestant
  columnValue _ _ = contestantName . rowContestant
  columnOrder column = compare `on` columnValue column (-1)
  columnValueDisplayer _ = toMarkup

newtype TotalSuccessesColumn = TotalSuccessesColumn { standing :: Standing }

instance StandingColumn TotalSuccessesColumn where
  type StandingColumnValue TotalSuccessesColumn = Integer
  columnTagClass = const "total_successes"
  columnCaptionText _ = "="
  columnCaptionTitleText TotalSuccessesColumn { standing = Standing {..} } =
    Just $ translate standingLanguage MsgSuccessesCaptionTitle
  columnValue _ _ = rowSuccesses . rowStats
  columnOrder column = compare `on` columnValue column (-1)
  columnValueDisplayer _ = toMarkup
  columnMaxValue =
    Just $ \TotalSuccessesColumn { standing = Standing {..} } -> toInteger . Map.size $ problems standingSource

newtype TotalAttemptsColumn = TotalAttemptsColumn { standing :: Standing }

instance StandingColumn TotalAttemptsColumn where
  type StandingColumnValue TotalAttemptsColumn = Integer
  columnTagClass = const "total_attempts"
  columnCaptionText _ = "!"
  columnCaptionTitleText TotalAttemptsColumn { standing = Standing {..} } =
    Just $ translate standingLanguage MsgAttemptsCaptionTitle
  columnValue _ _ = rowAttempts . rowStats
  columnOrder column = compare `on` columnValue column (-1)
  columnValueDisplayer _ = toMarkup

newtype TotalScoreColumn = TotalScoreColumn { standing :: Standing }

instance StandingColumn TotalScoreColumn where
  type StandingColumnValue TotalScoreColumn = Rational
  columnTagClass = const "total_score"
  columnCaptionText _ = preEscapedText "&Sigma;"
  columnCaptionTitleText TotalScoreColumn { standing = Standing {..} } =
    Just $ translate standingLanguage MsgTotalScoreCaptionTitle
  columnValue _ _ = rowScore . rowStats
  columnOrder column = compare `on` columnValue column (-1)
  columnValueDisplayer TotalScoreColumn {..} = displayRatio (fractionDisplayStyle . standingConfig $ standing)
  columnMaxValue =
    Just
      $ \TotalScoreColumn { standing = Standing { standingConfig = StandingConfig {..}, standingSource = StandingSource {..} } } ->
          fromInteger $ if enableScores then sum $ problemMaxScore <$> problems else toInteger $ Map.size problems

newtype LastSuccessTimeColumn = LastSuccessTimeColumn { standing :: Standing }

instance StandingColumn LastSuccessTimeColumn where
  type StandingColumnValue LastSuccessTimeColumn = Maybe UTCTime
  columnTagClass = const "last_success_time"
  columnCaptionText LastSuccessTimeColumn { standing = Standing {..} } =
    preEscapedText $ translate standingLanguage MsgLastSuccessTime
  columnCaptionTitleText LastSuccessTimeColumn { standing = Standing {..} } =
    Just $ translate standingLanguage MsgLastSuccessTimeCaptionTitle
  columnValue _ _ = rowLastTimeSuccess . rowStats
  columnOrder column = compare `on` columnValue column (-1)
  columnValueDisplayer _ Nothing     = ""
  columnValueDisplayer _ (Just time) = toMarkup time

data RatingProblemScoreColumn = RatingProblemScoreColumn { standing          :: !Standing
                                                         , contestantPrecalc :: !(Map Integer Double)
                                                         }

calculateProblemRating :: Standing -> Problem -> Double
calculateProblemRating Standing { standingConfig = StandingConfig {..}, ..} problem = case problemRatingFormula of
  Nothing -> 100
  (Just formula) ->
    let problemStat = case standingProblemStats !? getID problem of
          Nothing     -> mempty
          (Just stat) -> stat
        bindings =
            [ ELang.VariableBinding "successes" (return . ELang.ValueInt . problemSuccesses $ problemStat)
            , ELang.VariableBinding "overdueSuccesses" (return . ELang.ValueInt . problemOverdueSuccesses $ problemStat)
            ]
        result = case ELang.evaluate formula bindings of
          (Left  errorMsg) -> throw $ InvalidElangExpression errorMsg
          (Right value   ) -> case (ELang.fromValue value :: Maybe Double) of
            Nothing       -> throw $ DoubleValueExpected value
            (Just result) -> result
    in  result

calculateContestantRating :: Standing -> Map (Integer, Integer) Double -> StandingRow -> (Integer, Double)
calculateContestantRating standing problemRatings standingRow@StandingRow {..} =
  ( contestantID rowContestant
  , sum $ calculateContestantRatingOnProblem standing standingRow <$> Map.toList problemRatings
  )
 where
  calculateContestantRatingOnProblem :: Standing -> StandingRow -> ((Integer, Integer), Double) -> Double
  calculateContestantRatingOnProblem Standing { standingConfig = StandingConfig {..}, ..} StandingRow {..} (problemIDs, rating)
    = case rowCells !? problemIDs of
      Nothing                  -> 0
      (Just StandingCell {..}) -> baseScore * penalty
       where
        baseScore = if cellType == Success then rating else 0
        penalty   = if enableDeadlines && cellIsOverdue then fromRational deadlinePenalty else 1

mkRatingProblemScoreColumn :: Standing -> RatingProblemScoreColumn
mkRatingProblemScoreColumn standing@Standing { standingSource = StandingSource {..}, ..} =
  let problemPrecalc    = calculateProblemRating standing <$> problems
      contestantPrecalc = calculateContestantRating standing problemPrecalc <$> standingRows
  in  RatingProblemScoreColumn { standing = standing, contestantPrecalc = Map.fromList contestantPrecalc }

instance StandingColumn RatingProblemScoreColumn where
  type StandingColumnValue RatingProblemScoreColumn = Double
  columnTagClass = const "rating_problem_score"
  columnCaptionText _ = preEscapedText "📊"
  columnCaptionTitleText RatingProblemScoreColumn { standing = Standing {..} } =
    Just $ translate standingLanguage MsgRatingProblemScoreCaptionTitle
  columnValue RatingProblemScoreColumn {..} _ StandingRow {..} =
    case contestantPrecalc !? contestantID rowContestant of
      Nothing      -> 0
      (Just value) -> value
  columnOrder column = compare `on` columnValue column (-1)
  columnValueDisplayer _ rating = toMarkup $ displayDouble rating

getColumnByVariant :: Standing -> ColumnVariant -> GenericStandingColumn
getColumnByVariant standing columnV = case columnV of
  PlaceColumnVariant              -> GenericStandingColumn $ PlaceColumn standing
  UserIDColumnVariant             -> GenericStandingColumn $ UserIDColumn standing
  NameColumnVariant               -> GenericStandingColumn $ ContestantNameColumn standing
  SuccessesColumnVariant          -> GenericStandingColumn $ TotalSuccessesColumn standing
  AttemptsColumnVariant           -> GenericStandingColumn $ TotalAttemptsColumn standing
  ScoreColumnVariant              -> GenericStandingColumn $ TotalScoreColumn standing
  LastSuccessTimeColumnVariant    -> GenericStandingColumn $ LastSuccessTimeColumn standing
  RatingProblemScoreColumnVariant -> GenericStandingColumn $ mkRatingProblemScoreColumn standing

-- Conditional styles

data ConditionalStyleColumn c = ConditionalStyleColumn { standing          :: !Standing
                                                       , baseColumn        :: !c
                                                       , conditionalStyles :: ![ConditionalStyle]
                                                       }

data ConditionalStyleRuntimeException = InvalidElangExpression !Text
                                      | BoolValueExpected !ELang.Value
                                      | DoubleValueExpected !ELang.Value

instance Exception ConditionalStyleRuntimeException

instance Show ConditionalStyleRuntimeException where
  show (InvalidElangExpression e    ) = sconcat ["ELang Runtime Exception: ", e]
  show (BoolValueExpected      value) = sconcat ["Expected Bool value in ELang expression, but ", show value, " got"]
  show (DoubleValueExpected    value) = sconcat ["Expected Double value in ELang expression, but ", show value, " got"]

instance StandingColumn c => StandingColumn (ConditionalStyleColumn c) where
  type StandingColumnValue (ConditionalStyleColumn c) = StandingColumnValue c
  columnTagClass    = columnTagClass . baseColumn
  columnCaptionText = columnCaptionText . baseColumn
  columnValue column = columnValue (baseColumn column)
  columnOrder column = columnOrder (baseColumn column)
  columnValueDisplayer column = columnValueDisplayer (baseColumn column)
  columnMaxValue         = (. baseColumn) <$> columnMaxValue
  columnCaptionTitleText = columnCaptionTitleText . baseColumn
  columnCaptionTag column = columnCaptionTag (baseColumn column)
  columnValueTag column = columnValueTag (baseColumn column)
  columnCaption column = columnCaption (baseColumn column)
  columnValueCell ConditionalStyleColumn {..} place row = foldl (!) baseValueCell stylesToApply
   where
    stylesToApply =
      [ style (toValue styleValue)
      | ConditionalStyle {..} <- conditionalStyles
      , fromELangEvaluationToBool $ ELang.evaluate conditions columnBindings
      ]
    baseValueCell  = columnValueCell baseColumn place row
    columnBindings = do
      (name, variant) <- allColumnVariants
      let genericColumn = getColumnByVariant standing variant
      (bindName, bindValue) <- getValuesByGenericColumn genericColumn name place row
      [ELang.VariableBinding bindName (return bindValue)]

    fromELangEvaluationToBool :: Either Text ELang.Value -> Bool
    fromELangEvaluationToBool (Left  errorMsg) = throw $ InvalidElangExpression errorMsg
    fromELangEvaluationToBool (Right value   ) = case value of
      (ELang.ValueBool boolVal) -> boolVal
      _                         -> throw $ BoolValueExpected value

    getValuesByGenericColumn :: GenericStandingColumn -> Text -> Integer -> StandingRow -> [(Text, ELang.Value)]
    getValuesByGenericColumn (GenericStandingColumn column) name place row = mainValue : maxValue
     where
      mainValue = (nameLower, ELang.toValue $ columnValue column place row)
      nameLower = case Text.uncons name of
        Just (letter, rest) -> Text.cons (Char.toLower letter) rest
        Nothing             -> name
      maxValue  = case columnMaxValue of
        Nothing    -> []
        Just value -> [("max" <> name, ELang.toValue $ value column)]

getColumnByVariantWithStyles :: Standing -> ColumnVariant -> GenericStandingColumn
getColumnByVariantWithStyles standing@Standing { standingConfig = StandingConfig {..} } columnV =
  let baseColumn = getColumnByVariant standing columnV
  in  case Map.lookup columnV conditionalStyles of
        Nothing       -> baseColumn
        (Just []    ) -> baseColumn
        (Just styles) -> case baseColumn of
          (GenericStandingColumn column) -> GenericStandingColumn $ ConditionalStyleColumn standing column styles

-- Cell rendering

type CellContentBuilder = StandingCell -> Markup

scoreCellContent :: Standing -> CellContentBuilder
scoreCellContent Standing { standingConfig = StandingConfig { fractionDisplayStyle = style } } StandingCell {..} =
  if cellType == Ignore then mempty else span ! class_ "score" $ displayRatio style cellScore

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
  let total_minutes    = floor time `Prelude.div` 60 :: Integer
      (hours, minutes) = total_minutes `divMod` 60
  in  toMarkup (printf "%d:%02d" hours minutes :: String)

successTimeCellContent :: CellContentBuilder
successTimeCellContent StandingCell {..} = if cellType /= Success
  then mempty
  else case cellMainRun of
    Nothing       -> mempty
    Just Run {..} -> span ! class_ "success_time" $ displayContestTime $ runTime `diffUTCTime` cellStartTime

selectAdditionalCellContentBuilders :: Standing -> [CellContentBuilder]
selectAdditionalCellContentBuilders st@Standing { standingConfig = StandingConfig {..}, ..} = mconcat
  [ enableScores ==> scoreCellContent st
  , showAttemptsNumber ==> if enableScores then attemptsCellContent else wrongAttemptsCellContent
  , showSuccessTime ==> successTimeCellContent
  ]

buildCellTitle :: Standing -> StandingRow -> Problem -> StandingCell -> Text
buildCellTitle Standing { standingConfig = StandingConfig {..}, standingSource = StandingSource {..}, ..} StandingRow {..} Problem {..} StandingCell {..}
  = Text.intercalate ", " $ mconcat
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
renderProblemSuccesses Standing {..} problem =
  td ! class_ "problem_successes row_value" $ toMarkup $ case standingProblemStats !? getID problem of
    (Just stats) -> problemSuccesses stats
    _            -> 0

renderStandingProblemSuccesses :: Standing -> Markup
renderStandingProblemSuccesses standing@Standing {..} =
  let header =
          td
            ! class_ "problem_successes row_header"
            ! colspan (toValue . length $ standingColumns)
            $ preEscapedText
            $ translate standingLanguage MsgCorrectSolutions
  in  tr $ foldl (>>) header $ renderProblemSuccesses standing <$> standingProblems
