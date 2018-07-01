module Ejudge.Standings.DataParser (parseEjudgeXML) where

import Text.XML.HXT.Core
import Ejudge.Standings.BaseModels
import Ejudge.Standings.StandingsModels (StandingsSource(..))
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale, addUTCTime, NominalDiffTime(..))
import Data.Fixed (Pico(..))

getRunlog :: ArrowXml a => a XmlTree XmlTree
getRunlog = getChildren >>> isElem >>> hasName "runlog"

readContest :: XmlTree -> Contest
readContest runlog = Contest cid cname cstime cschtime
  where
    cid = read . head $ runLA (getAttrValue0 "contest_id") runlog
    cname = head $ runLA getContestName runlog
    cstime = toUTC . head $ runLA (getAttrValue "start_time") runlog
    cschtime = toUTC . head $ runLA (getAttrValue "sched_start_time") runlog
    toUTC = parseTimeM True defaultTimeLocale "%Y/%m/%d %T"
    getContestName = getChildren >>> isElem >>> hasName "name" >>> getChildren >>> isText >>> getText
    
getChild :: ArrowXml a => String -> a XmlTree XmlTree
getChild child = getChildren >>> isElem >>> hasName child

filterItems :: ArrowXml a => String -> a XmlTree XmlTree
filterItems s = getChild (s ++ "s") >>> getChild s

readContestants :: XmlTree -> [Contestant]
readContestants = runLA $ filterItems "user" >>>
  (getAttrValue0 "id" >>> arr read) &&& getAttrValue0 "name" >>>
  arr (uncurry Contestant)

readLanguages :: XmlTree -> [Language]
readLanguages = runLA $ filterItems "language" >>>
  ((getAttrValue0 "id" >>> arr read) &&& getAttrValue0 "short_name") &&& getAttrValue0 "long_name" >>>
  arr (uncurry . uncurry $ Language)

readProblems :: Contest -> XmlTree -> [Problem]
readProblems contest = runLA $ filterItems "problem" >>>
  ((getAttrValue0 "id" >>> arr read) &&& getAttrValue0 "short_name") &&& getAttrValue0 "long_name" >>>
  arr (uncurry . uncurry $ (flip Problem) contest)


makeContestTime :: Maybe UTCTime -> (Double, Double) -> UTCTime
makeContestTime Nothing _ = error "Contest has no start time, but runs exist"
makeContestTime (Just time) (sec, nsec) = addUTCTime (realToFrac (nsec / 1000000000 + sec)) time


readRuns :: Contest -> [Contestant] -> [Language] -> [Problem] -> XmlTree -> [Run]
readRuns contest users langs probs runlog = map makeRun $ runLA (filterItems "run") runlog
  where
    makeRun run = Run runID contest runContestant runProblem runTime runStatus runLanguage runScore runTest
      where
        runID = read $ retrieve "run_id"
        runContestant = findByAttribute0 contestantID users . read $ retrieve "user_id"
        runProblem = findByAttribute problemID probs $ retrieveM "prob_id" >>= Just . read
        runTime = makeContestTime (contestStartTime contest) $ (read $ retrieve "time", read $ retrieve "nsec") 
        runStatus = read $ retrieve "status"
        runLanguage = findByAttribute languageID langs $ retrieveM "lang_id" >>= Just . read
        runScore = retrieveM "score" >>= Just . read
        runTest = retrieveM "test" >>= Just . read
        retrieve attr = head $ runLA (getAttrValue0 attr) run
        retrieveM attr = ifNonEmpty . head $ runLA (getAttrValue attr) run
          where
            ifNonEmpty s = case s of { "" -> Nothing; _ -> Just s }
        

parseEjudgeXML :: FilePath -> IO StandingsSource
parseEjudgeXML file = do
  (runlog:_) <- runX $ readDocument [] file >>> getRunlog
  let contest = readContest runlog
      users = readContestants runlog
      langs = readLanguages runlog
      probs = readProblems contest runlog
      runs = readRuns contest users langs probs runlog
  return $ StandingsSource [contest] users langs probs runs 
