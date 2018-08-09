{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}
module EjStand.StandingBuilder
  ( takeFromSetBy
  , prepareStandingSource
  , buildStanding
  )
where

import           Data.List              (sortOn)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (catMaybes, fromMaybe)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (unpack)
import           Data.Time              (UTCTime)
import           EjStand.BaseModels
import           EjStand.DataParser
import           EjStand.StandingModels
import           Safe                   (headMay, lastMay)
import           Text.Printf            (printf)

-- Utilities

takeFromSetBy :: Ord b => (a -> b) -> b -> Set a -> Set a
takeFromSetBy f x = Set.takeWhileAntitone ((== x) . f) . Set.dropWhileAntitone ((< x) . f)

-- Preparing data IO operations

prepareStandingSource :: GlobalConfiguration -> StandingConfig -> IO StandingSource
prepareStandingSource GlobalConfiguration {..} StandingConfig {..} =
  parseEjudgeXMLs $ map (printf (unpack $ xmlFilePattern)) $ Set.toList $ standingContests

-- Deadlines computations

isAppliableDeadlineOption :: Problem -> Contestant -> StandingOption -> Bool
isAppliableDeadlineOption Problem {..} Contestant {..} SetFixedDeadline {..}
  | not $ Set.member problemContest contestIDs = False
  | otherwise = case contestantIDs of
    Nothing  -> True
    Just ids -> Set.member contestantID ids
isAppliableDeadlineOption _ _ _ = False

calculateDeadline :: StandingConfig -> StandingSource -> Problem -> Contestant -> Maybe UTCTime
calculateDeadline StandingConfig {..} StandingSource {..} prob@Problem {..} user@Contestant {..} =
  if (elem EnableDeadlines standingOptions)
    then
      let nextContest     = Set.lookupMin $ Set.dropWhileAntitone ((<= problemContest) . contestID) contests
          defaultDeadline = nextContest >>= contestStartTime
          customDeadline  = fmap deadline $ lastMay $ filter (isAppliableDeadlineOption prob user) $ standingOptions
      in  headMay $ catMaybes [customDeadline, defaultDeadline]
    else Nothing

getDeadlinePenalty :: StandingConfig -> Rational
getDeadlinePenalty cfg = fromMaybe 1 $ headMay $ [ penalty | SetDeadlinePenalty penalty <- standingOptions cfg ]

-- Standing building

defaultCell :: StandingCell
defaultCell =
  StandingCell {cellType = Ignore, cellIsOverdue = False, cellScore = 0, cellAttempts = 0, cellMainRun = Nothing}

applyRunDeadline :: Maybe (UTCTime, Rational) -> Run -> (Run, Bool)
applyRunDeadline Nothing run = (run, False)
applyRunDeadline (Just (time, penalty)) run@Run {..}
  | runTime < time = (run, False)
  | otherwise      = (run { runScore = runScore >>= return . (* penalty) }, True)

getRunScore :: StandingConfig -> (Run, Bool) -> Rational
getRunScore cfg@(elem EnableScores . standingOptions -> True) ((runScore -> Nothing), _)    = 0
getRunScore cfg@(elem EnableScores . standingOptions -> True) ((runScore -> Just score), _) = score
getRunScore cfg ((getRunStatusType . runStatus -> Success), False)                          = 1
getRunScore cfg ((getRunStatusType . runStatus -> Success), True)                           = getDeadlinePenalty cfg
getRunScore _   _                                                                           = 0

recalculateCellAttempts :: StandingConfig -> (Run, Bool) -> StandingCell -> StandingCell
recalculateCellAttempts _ runT@(run@Run {..}, overdue) cell@StandingCell {..}
  | cellType >= Pending                   = cell
  | getRunStatusType runStatus /= Mistake = cell
  | otherwise                             = cell { cellAttempts = cellAttempts + 1 }

setCellMainRun :: Bool -> StandingConfig -> (Run, Bool) -> StandingCell -> StandingCell
setCellMainRun forceFlag cfg@StandingConfig {..} runT@(run@Run {..}, overdue) cell@StandingCell {..} =
  let score = getRunScore cfg runT
      cell' = recalculateCellAttempts cfg runT cell
  in  if forceFlag || elem OnlyScoreLastSubmit standingOptions || cellScore < score
        then cell' { cellType      = getRunStatusType runStatus
                   , cellIsOverdue = overdue
                   , cellMainRun   = Just run
                   , cellScore     = score
                   }
        else if (getRunStatusType runStatus > cellType)
          then cell' { cellType = getRunStatusType runStatus, cellIsOverdue = overdue, cellMainRun = Just run }
          else cell'

setCellMainRunForce :: StandingConfig -> (Run, Bool) -> StandingCell -> StandingCell
setCellMainRunForce = setCellMainRun True

setCellMainRunMaybe :: StandingConfig -> (Run, Bool) -> StandingCell -> StandingCell
setCellMainRunMaybe = setCellMainRun False

applicateRun :: StandingConfig -> (Run, Bool) -> StandingCell -> StandingCell
-- 0 priority: Ignore
applicateRun _ ((getRunStatusType . runStatus -> Ignore), _) cell              = cell
applicateRun cfg runT cell@StandingCell { cellType = Ignore, ..}               = setCellMainRunForce cfg runT cell
-- 1 priority: Error
applicateRun _ _ cell@StandingCell { cellType = Error, ..}                     = cell
applicateRun cfg runT@((getRunStatusType . runStatus -> Error), _) cell        = setCellMainRunForce cfg runT cell
-- 2 priority: Disqualified
applicateRun _ _ cell@StandingCell { cellType = Disqualified, ..}              = cell
applicateRun cfg runT@((getRunStatusType . runStatus -> Disqualified), _) cell = setCellMainRunForce cfg runT cell
-- Extra priorities: Other statuses
applicateRun cfg runT cell                                                     = setCellMainRunMaybe cfg runT cell

buildCell :: StandingConfig -> StandingSource -> Problem -> Contestant -> StandingCell
buildCell cfg@StandingConfig {..} src@StandingSource {..} prob@Problem {..} user@Contestant {..} =
  let runsList  = filter ((== contestantID) . runContestant) $ Set.toList $ takeFromSetBy runContest problemContest runs
      deadline  = calculateDeadline cfg src prob user
      penalty   = getDeadlinePenalty cfg
      deadlineT = (\x -> (x, penalty)) <$> deadline
  in  foldl (flip $ applicateRun cfg) defaultCell $ fmap (applyRunDeadline deadlineT) $ runsList

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
buildRows cfg src probs = buildRow cfg src probs <$> (Set.toList $ contestants src)

buildProblems :: StandingConfig -> StandingSource -> [Problem]
buildProblems cfg | elem ReversedContestOrder $ standingOptions cfg = sortOn cmp . Set.toList . problems
                  | otherwise = Set.toList . problems
  where cmp = ([negate . problemContest, problemID] <*>) . return

sortRows :: [StandingRow] -> [StandingRow]
sortRows = sortOn (comparator . calculateRowStats)
 where
  comparator :: StandingRowStats -> (Rational, Integer, Maybe UTCTime)
  comparator StandingRowStats {..} = (negate rowScore, negate rowSuccesses, rowLastTimeSuccess)

buildStanding :: StandingConfig -> StandingSource -> Standing
buildStanding cfg src =
  let problems = buildProblems cfg src
      rows     = sortRows $ buildRows cfg src problems
  in  Standing {standingConfig = cfg, standingSource = src, standingProblems = problems, standingRows = rows}
