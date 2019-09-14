{-# LANGUAGE RecordWildCards #-}
module EjStand.StandingBuilder
  ( prepareStandingSource
  , buildStanding
  )
where

import           Data.Functor                   ( (<&>) )
import           Data.List                      ( foldl'
                                                , sortBy
                                                , sortOn
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map
                                                , (!)
                                                , (!?)
                                                )
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                , fromMaybe
                                                )
import           Data.Ratio                     ( (%) )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Time                      ( UTCTime )
import           EjStand.Internals.Core         ( fromIdentifiableList )
import           EjStand.Models.Base
import           EjStand.Models.Standing
import           EjStand.Parsers.Data           ( parseEjudgeXMLs )
import           EjStand.Parsers.EjudgeOptions  ( updateStandingSourceWithProblemConfigurations )
import           EjStand.Web.HtmlElements       ( getColumnByVariant
                                                , getColumnByVariantWithStyles
                                                )
import           Safe                           ( headMay
                                                , lastMay
                                                , minimumMay
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

defaultCell :: Contest -> StandingCell
defaultCell Contest {..} = StandingCell
  { cellType      = Ignore
  , cellIsOverdue = False
  , cellScore     = 0
  , cellAttempts  = 0
  , cellMainRun   = Nothing
  , cellStartTime = fromJust contestStartTime
  }

applyRunDeadline :: Maybe UTCTime -> Run -> (Run, Bool)
applyRunDeadline Nothing     run          = (run, False)
applyRunDeadline (Just time) run@Run {..} = (run, runTime >= time)

getRunScore :: StandingConfig -> Problem -> (Run, Bool) -> Integer -> Rational
getRunScore StandingConfig {..} Problem {..} (Run {..}, overdue) attempts =
  let deadlineMultiplier = if enableDeadlines && overdue then deadlinePenalty else 1
      (contextMaxScore, runPenalty, fixedScore) =
        if enableScores then (problemMaxScore, attempts * problemRunPenalty, runScore) else (1, 0, Nothing)
      statusBasedRunScore = case getRunStatusType runStatus of
        Success -> contextMaxScore
        _       -> 0
      score               = fromMaybe (statusBasedRunScore % 1) fixedScore
  in  0 `max` score * deadlineMultiplier - runPenalty % 1

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

applicateRun :: StandingConfig -> Problem -> StandingCell -> (Run, Bool) -> StandingCell
applicateRun cfg prob cell@StandingCell {..} runT@(Run {..}, _)
  | getRunStatusType runStatus == Ignore       = cell
  | cellType == Error                          = cell
  | getRunStatusType runStatus == Error        = (setCellMainRunForce cfg prob runT cell) { cellScore = 0 }
  | cellType == Disqualified                   = cell
  | getRunStatusType runStatus == Disqualified = (setCellMainRunForce cfg prob runT cell) { cellScore = 0 }
  | otherwise                                  = setCellMainRunMaybe cfg prob runT cell

getVirtualStart :: StandingSource -> Problem -> Contestant -> Maybe UTCTime
getVirtualStart StandingSource {..} Problem {..} Contestant {..} = minimumMay $ runTime <$> filter
  ((== VS) . runStatus)
  (Map.elems $ filterRunMap problemContest contestantID Nothing runs)

buildCell :: StandingConfig -> StandingSource -> Problem -> Contestant -> StandingCell
buildCell cfg@StandingConfig {..} src@StandingSource {..} prob@Problem {..} user@Contestant {..} =
  let runsList     = filterRunMap problemContest contestantID (Just problemID) runs
      deadline     = calculateDeadline cfg src prob user
      startCell    = defaultCell $ contests ! problemContest
      virtualStart = if showSuccessTime then getVirtualStart src prob user else Nothing
      cell         = foldl (applicateRun cfg prob) startCell $ applyRunDeadline deadline <$> runsList
  in  cell { cellStartTime = fromMaybe (cellStartTime cell) virtualStart }

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

buildRow :: Standing -> Contestant -> StandingRow
buildRow Standing {..} user = appendRecalculatedCellStats $ StandingRow
  { rowContestant = user
  , rowCells      = Map.fromList $ fmap
    (\p@Problem {..} -> ((problemContest, problemID), buildCell standingConfig standingSource p user))
    standingProblems
  , rowStats      = mempty
  }

buildRows :: Standing -> [StandingRow]
buildRows standing@Standing {..} = buildRow standing <$> Map.elems (contestants standingSource)

buildProblems :: StandingConfig -> StandingSource -> [Problem]
buildProblems StandingConfig {..} | reversedContestOrder = sortOn reversedCmp . elementsF
                                  | otherwise            = elementsF
 where
  reversedCmp = ([negate . problemContest, problemID] <*>) . return
  elementsF   = Map.elems . problems

sortRows :: [(OrderType, GenericStandingColumn)] -> [StandingRow] -> [StandingRow]
sortRows orderer = sortBy (comparator orderer)
 where
  comparator :: [(OrderType, GenericStandingColumn)] -> StandingRow -> StandingRow -> Ordering
  comparator [] _ _ = EQ
  comparator ((ord, GenericStandingColumn column) : tail) row1 row2 =
    let result         = columnOrder column row1 row2
        resultWithType = case ord of
          Ascending  -> result
          Descending -> compare EQ result
    in  case resultWithType of
          EQ -> comparator tail row1 row2
          x  -> x

mergeStandingSourceContestantsByName :: StandingSource -> StandingSource
mergeStandingSourceContestantsByName src@StandingSource {..} = src { contestants = newContestants, runs = newRuns }
 where
  (_, replacerMap, newContestants) =
    foldl' contestantFoldFunction (Map.empty, Map.empty, contestants) $ Map.elems contestants
  newRuns =
    fromIdentifiableList
      $   Map.elems runs
      <&> (\run@Run {..} -> case replacerMap !? runContestant of
            Just newContestant -> run { runContestant = newContestant }
            Nothing            -> run
          )
  contestantFoldFunction
    :: (Map Text Integer, Map Integer Integer, Map Integer Contestant)
    -> Contestant
    -> (Map Text Integer, Map Integer Integer, Map Integer Contestant)
  contestantFoldFunction (nameIndex, idIndex, contestantMap) c1 = case nameIndex !? contestantName c1 of
    Nothing     -> (Map.insert (contestantName c1) (contestantID c1) nameIndex, idIndex, contestantMap)
    (Just c2id) -> (nameIndex, Map.insert (contestantID c1) c2id idIndex, Map.delete (contestantID c1) contestantMap)

buildProblemStats :: Standing -> Map (Integer, Integer) StandingProblemStats
buildProblemStats Standing {..} = Map.fromListWith
  (<>)
  (cellToProblemStat <$> concat (Map.toList . rowCells <$> standingRows))
 where
  cellToProblemStat :: (a, StandingCell) -> (a, StandingProblemStats)
  cellToProblemStat (id, StandingCell {..}) =
    ( id
    , StandingProblemStats
      { problemSuccesses        = if cellType == Success then 1 else 0
      , problemOverdueSuccesses = if cellType == Success && cellIsOverdue then 1 else 0
      }
    )

buildStanding :: [Lang] -> StandingConfig -> StandingSource -> Standing
buildStanding lang cfg@StandingConfig {..} src =
  let src'           = if mergeContestantsByName then mergeStandingSourceContestantsByName src else src
      problems       = buildProblems cfg src'

      -- Stage 1. No rows, no columns, no problem stats
      standingStage1 = Standing
        { standingLanguage     = lang
        , standingConfig       = cfg
        , standingSource       = src'
        , standingProblems     = problems
        , standingRows         = mempty
        , standingColumns      = mempty
        , standingProblemStats = mempty
        }

      unsortedRows   = buildRows standingStage1

      -- Stage II. Unsorted rows, no columns, no problem stats
      standingStage2 = standingStage1 { standingRows = unsortedRows }

      problemStats   = buildProblemStats standingStage2

      -- Stage III. Unsorted rows, no columns
      standingStage3 = standingStage2 { standingProblemStats = problemStats }

      orderer        = (\(ord, variant) -> (ord, getColumnByVariant standingStage3 variant)) <$> rowSortingOrder
      rows           = sortRows orderer unsortedRows

      -- Stage IV. No columns, no problem stats
      standingStage4 = standingStage3 { standingRows = rows }

      columns        = getColumnByVariantWithStyles standingStage4 <$> displayedColumns

      -- Stage IV. Final
      standingStage5 = standingStage4 { standingColumns = columns }
  in  standingStage5
