{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}
module EjStand.StandingBuilder
  ( prepareStandingSource
  , buildStanding
  )
where

import           Data.List                      ( sortOn )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( catMaybes )
import           Data.Ratio                     ( (%) )
import qualified Data.Set                      as Set
import           Data.Text                      ( unpack )
import           Data.Time                      ( UTCTime )
import           EjStand.Internals.Core         ( (==>) )
import           EjStand.Models.Base
import           EjStand.Models.Standing
import           EjStand.Parsers.Data           ( parseEjudgeXMLs )
import           EjStand.Parsers.EjudgeOptions  ( updateStandingSourceWithProblemConfigurations )
import           EjStand.Web.HtmlElements
import           Safe                           ( headMay
                                                , lastMay
                                                )
import           Text.Printf                    ( printf )
import           Text.Shakespeare.I18N          ( Lang )

-- Preparing data IO operations

prepareStandingSource :: GlobalConfiguration -> StandingConfig -> IO StandingSource
prepareStandingSource global@GlobalConfiguration {..} StandingConfig {..} = do
  src <- parseEjudgeXMLs $ map (printf (unpack xmlFilePattern)) $ Set.toList standingContests
  if enableScores then updateStandingSourceWithProblemConfigurations global src else return src

-- Deadlines computations

isAppliableDeadlineOption :: Problem -> Contestant -> FixedDeadline -> Bool
isAppliableDeadlineOption Problem {..} Contestant {..} FixedDeadline {..}
  | Set.member problemContest contestIDs = case contestantIDs of
    Nothing  -> True
    Just ids -> Set.member contestantID ids
  | otherwise = False

calculateDeadline :: StandingConfig -> StandingSource -> Problem -> Contestant -> Maybe UTCTime
calculateDeadline StandingConfig {..} StandingSource {..} prob@Problem {..} user@Contestant {..} = if enableDeadlines
  then
    let nextContest     = snd <$> Map.lookupGT problemContest contests
        defaultDeadline = nextContest >>= contestStartTime
        customDeadline  = fmap deadline $ lastMay $ filter (isAppliableDeadlineOption prob user) fixedDeadlines
    in  headMay $ catMaybes [customDeadline, defaultDeadline]
  else Nothing

-- Standing building

defaultCell :: StandingCell
defaultCell =
  StandingCell { cellType = Ignore, cellIsOverdue = False, cellScore = 0, cellAttempts = 0, cellMainRun = Nothing }

applyRunDeadline :: Maybe (UTCTime, Rational) -> Run -> (Run, Bool)
applyRunDeadline Nothing run = (run, False)
applyRunDeadline (Just (time, penalty)) run@Run {..} | runTime < time = (run, False)
                                                     | otherwise = (run { runScore = (* penalty) <$> runScore }, True)

getRunScore :: StandingConfig -> Problem -> (Run, Bool) -> Integer -> Rational
getRunScore StandingConfig {..} Problem {..} (Run {..}, overdue) attempts =
  (if enableDeadlines && overdue then deadlinePenalty else 1) * if enableScores
    then case runScore of
      Nothing      -> 0
      (Just score) -> max 0 (score - attempts * problemRunPenalty % 1)
    else case getRunStatusType runStatus of
      Success -> 1
      _       -> 0

recalculateCellAttempts :: StandingConfig -> (Run, Bool) -> StandingCell -> StandingCell
recalculateCellAttempts _ (Run {..}, _) cell@StandingCell {..} | cellType >= Pending = cell
                                                               | getRunStatusType runStatus /= Mistake = cell
                                                               | otherwise = cell { cellAttempts = cellAttempts + 1 }

setCellMainRun :: Bool -> StandingConfig -> Problem -> (Run, Bool) -> StandingCell -> StandingCell
setCellMainRun forceFlag cfg@StandingConfig {..} prob runT@(run@Run {..}, overdue) cell@StandingCell {..} =
  let score = getRunScore cfg prob runT cellAttempts
      cell' = recalculateCellAttempts cfg runT cell
  in  if forceFlag || onlyScoreLastSubmit || cellScore < score
        then cell' { cellType      = getRunStatusType runStatus
                   , cellIsOverdue = overdue
                   , cellMainRun   = Just run
                   , cellScore     = score
                   }
        else if getRunStatusType runStatus > cellType
          then cell' { cellType = getRunStatusType runStatus, cellIsOverdue = overdue, cellMainRun = Just run }
          else cell'

setCellMainRunForce :: StandingConfig -> Problem -> (Run, Bool) -> StandingCell -> StandingCell
setCellMainRunForce = setCellMainRun True

setCellMainRunMaybe :: StandingConfig -> Problem -> (Run, Bool) -> StandingCell -> StandingCell
setCellMainRunMaybe = setCellMainRun False

applicateRun :: StandingConfig -> Problem -> (Run, Bool) -> StandingCell -> StandingCell
-- 0 priority: Ignore
applicateRun _ _ (getRunStatusType . runStatus -> Ignore, _) cell = cell
-- 1 priority: Error
applicateRun _ _ _ cell@StandingCell { cellType = Error, ..} = cell
applicateRun cfg prob runT@(getRunStatusType . runStatus -> Error, _) cell =
  (setCellMainRunForce cfg prob runT cell) { cellScore = 0 }
-- 2 priority: Disqualified
applicateRun _ _ _ cell@StandingCell { cellType = Disqualified, ..} = cell
applicateRun cfg prob runT@(getRunStatusType . runStatus -> Disqualified, _) cell =
  (setCellMainRunForce cfg prob runT cell) { cellScore = 0 }
-- Extra priorities: Other statuses
applicateRun cfg prob runT cell = setCellMainRunMaybe cfg prob runT cell

buildCell :: StandingConfig -> StandingSource -> Problem -> Contestant -> StandingCell
buildCell cfg@StandingConfig {..} src@StandingSource {..} prob@Problem {..} user@Contestant {..} =
  let runsList  = filterRunMap problemContest contestantID (Just problemID) runs
      deadline  = calculateDeadline cfg src prob user
      deadlineT = (, deadlinePenalty) <$> deadline
  in  foldl (flip $ applicateRun cfg prob) defaultCell $ applyRunDeadline deadlineT <$> runsList

calculateCellStats :: StandingCell -> StandingRowStats
calculateCellStats StandingCell {..} = if cellType /= Success
  then mempty { rowScore = cellScore }
  else StandingRowStats
    { rowSuccesses       = 1
    , rowAttempts        = cellAttempts
    , rowScore           = cellScore
    , rowLastTimeSuccess = runTime <$> cellMainRun
    }

calculateRowStats :: StandingRow -> StandingRowStats
calculateRowStats StandingRow {..} = mconcat $ calculateCellStats <$> Map.elems rowCells

appendRecalculatedCellStats :: StandingRow -> StandingRow
appendRecalculatedCellStats row = row { rowStats = calculateRowStats row }

buildRow :: StandingConfig -> StandingSource -> [Problem] -> Contestant -> StandingRow
buildRow cfg src probs user = appendRecalculatedCellStats $ StandingRow
  { rowContestant = user
  , rowCells = Map.fromList $ fmap (\p@Problem {..} -> ((problemContest, problemID), buildCell cfg src p user)) probs
  , rowStats = mempty
  }

buildRows :: StandingConfig -> StandingSource -> [Problem] -> [StandingRow]
buildRows cfg src probs = buildRow cfg src probs <$> Map.elems (contestants src)

buildProblems :: StandingConfig -> StandingSource -> [Problem]
buildProblems (reversedContestOrder -> True) = sortOn cmp . Map.elems . problems
  where cmp = ([negate . problemContest, problemID] <*>) . return
buildProblems _ = Map.elems . problems

sortRows :: [StandingRow] -> [StandingRow]
sortRows = sortOn (comparator . calculateRowStats)
 where
  comparator :: StandingRowStats -> (Rational, Integer, Maybe UTCTime)
  comparator StandingRowStats {..} = (negate rowScore, negate rowSuccesses, rowLastTimeSuccess)

buildColumns :: [Lang] -> StandingConfig -> StandingSource -> [StandingColumn]
buildColumns lang cfg@StandingConfig {..} src = mconcat
  [ [placeColumn lang, contestantNameColumn lang, totalScoreColumn cfg src]
  , (enableDeadlines || enableScores) ==> totalSuccessesColumn
  , [lastSuccessTimeColumn lang]
  ]

buildStanding :: [Lang] -> StandingConfig -> StandingSource -> Standing
buildStanding lang cfg src =
  let problems = buildProblems cfg src
  in  Standing
        { standingConfig   = cfg
        , standingSource   = src
        , standingProblems = problems
        , standingRows     = sortRows $ buildRows cfg src problems
        , standingColumns  = buildColumns lang cfg src
        }
