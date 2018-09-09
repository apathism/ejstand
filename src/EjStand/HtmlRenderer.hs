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
import           Data.Maybe                    (catMaybes)
import           Data.Ratio                    (Ratio, denominator, numerator, (%))
import qualified Data.Set                      as Set
import           Data.String                   (IsString)
import           Data.Text                     (Text, intercalate, splitOn)
import qualified Data.Text.Internal.Lazy       as LT
import           Data.Time                     (UTCTime, defaultTimeLocale)
import           Data.Time.Format              (formatTime)
import           EjStand                       (getVersion)
import           EjStand.BaseModels
import           EjStand.InternalsCore         (takeFromSetBy, (==>))
import           EjStand.StandingModels
import           Prelude                       hiding (div, span)
import qualified Prelude                       (div)
import           Text.Blaze.Html               (Markup, ToMarkup, preEscapedToMarkup, toMarkup)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              hiding (style, title)
import           Text.Blaze.Html5.Attributes   hiding (span)
import           Text.Hamlet                   (shamletFile)
import           Text.Lucius                   (luciusFile, renderCss)

-- Utilities for templates

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [1 ..]

buildContestList :: StandingSource -> [Problem] -> [(Maybe Contest, Int)]
buildContestList StandingSource {..} problemList =
  groupSame $ Set.lookupMin <$> flip (takeFromSetBy contestID) contests <$> problemContest <$> problemList

groupSame :: Eq a => [a] -> [(a, Int)]
groupSame []           = []
groupSame (fst : tail) = let (e, tail') = takeSame fst 1 $! tail in (e : (groupSame $! tail'))
 where
  takeSame :: Eq a => a -> Int -> [a] -> ((a, Int), [a])
  takeSame x k [] = ((x, k), [])
  takeSame x k lst@(e : tail) | x == e    = takeSame x (k + 1) $! tail
                              | otherwise = ((x, k), lst)

getRowCellByProblem :: StandingRow -> Problem -> (Problem, StandingCell)
getRowCellByProblem row@StandingRow {..} prob@Problem {..} = case Map.lookup (problemContest, problemID) rowCells of
  (Just cell) -> (prob, cell)
  Nothing     -> error $ "Can't find standing cell for " ++ show prob ++ " in " ++ show row

getTextVersion :: Text
getTextVersion = getVersion

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

calculateConditionalStyle :: [StandingOption] -> Rational -> (Html -> Html) -> Html -> Html
calculateConditionalStyle [] _ html = html
calculateConditionalStyle ((ConditionalStyle conditions styleValue) : tail) value html
  | checkComparison value `all` conditions = html ! style (toValue styleValue)
  | otherwise                              = calculateConditionalStyle tail value html
calculateConditionalStyle (_ : tail) value html = calculateConditionalStyle tail value html

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

totalScoreColumn :: StandingConfig -> StandingColumn
totalScoreColumn StandingConfig {..} = StandingColumn caption value
 where
  caption = th ! class_ "total_score" ! rowspan "2" $ preEscapedToMarkup ("&Sigma;" :: Text)
  value (_, StandingRow {..}) =
    calculateConditionalStyle standingOptions relativeScore td ! class_ "total_score" $ toMarkup score
   where
    score         = rowScore rowStats
    -- This is a very bold assumption, but we can't get more information through ejudge XML interface
    maxScore      = toInteger $ if elem EnableScores standingOptions then Map.size rowCells * 100 else Map.size rowCells
    relativeScore = score / (maxScore % 1)

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
  [ elem EnableScores standingOptions ==> scoreCellContent
  , elem ShowAttemptsNumber standingOptions
    ==> if elem EnableScores standingOptions then attemptsCellContent else wrongAttemptsCellContent
  ]

buildCellTitle :: Standing -> StandingRow -> Problem -> StandingCell -> Text
buildCellTitle Standing { standingConfig = StandingConfig {..}, standingSource = StandingSource {..}, ..} StandingRow {..} Problem {..} StandingCell {..}
  = intercalate ", " $ mconcat
    [ [contestantName rowContestant, mconcat [problemShortName, " (", problemLongName, ")"]]
    , catMaybes
    $   elem ShowLanguages standingOptions
    ==> (languageLongName <$> (cellMainRun >>= runLanguage >>= findLanguageByID))
    ]
  where findLanguageByID id = Set.lookupMin $ takeFromSetBy languageID id languages

renderCell :: Standing -> StandingRow -> Problem -> CellContentBuilder
renderCell st@Standing { standingConfig = StandingConfig {..}, ..} row problem cell@StandingCell {..} =
  cellTag' $ foldl (>>) cellValue additionalContent
 where
  additionalContent = if allowCellContent then selectAdditionalCellContentBuilders st <*> [cell] else []
  addRunStatusCellText text = span ! class_ "run_status" $ text
  ifNotScores x = if elem EnableScores standingOptions then mempty else x
  cellTag'                               = cellTag ! title (toValue $ buildCellTitle st row problem cell)
  (cellTag, cellValue, allowCellContent) = case cellType of
    Success -> case cellIsOverdue of
      False -> (td ! class_ "success", ifNotScores $ addRunStatusCellText "+", True)
      True  -> (td ! class_ "overdue", ifNotScores $ addRunStatusCellText "+.", True)
    Processing   -> (td ! class_ "processing", ifNotScores $ addRunStatusCellText "-", True)
    Pending      -> (td ! class_ "pending", ifNotScores $ addRunStatusCellText "?", True)
    Rejected     -> (td ! class_ "rejected", ifNotScores $ addRunStatusCellText "-", True)
    Mistake      -> (td ! class_ "mistake", ifNotScores $ addRunStatusCellText "-", True)
    Ignore       -> (td ! class_ "none", "", True)
    Disqualified -> (td ! class_ "disqualified", "", False)
    Error        -> (td ! class_ "error", addRunStatusCellText "✖", False)

renderProblemSuccesses :: Standing -> Problem -> Markup
renderProblemSuccesses Standing {..} prob@Problem {..} =
  let countProblemSuccesses =
        length
          .   filter ((== Success) . cellType)
          .   catMaybes
          $   Map.lookup (problemContest, problemID)
          .   rowCells
          <$> standingRows
  in  td ! class_ "problem_successes row_value" $ toMarkup countProblemSuccesses

renderStandingProblemSuccesses :: Standing -> Markup
renderStandingProblemSuccesses standing@Standing {..} =
  let header =
        td
          ! class_ "problem_successes row_header"
          ! colspan (toValue . length $ standingColumns)
          $ "Правильных решений:"
  in  foldl (>>) header $ renderProblemSuccesses standing <$> standingProblems

-- Main entry points

renderStanding :: GlobalConfiguration -> Standing -> LT.Text
renderStanding GlobalConfiguration {..} standing@Standing { standingConfig = StandingConfig {..}, ..} =
  let problemSuccesses = elem ShowProblemStatistics standingOptions ==> renderStandingProblemSuccesses standing
  in  renderHtml ($(shamletFile "templates/main.hamlet"))

renderCSS :: LT.Text
renderCSS = renderCss ($(luciusFile "templates/main.lucius") undefined)
