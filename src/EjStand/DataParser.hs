{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module EjStand.DataParser
  ( ParsingException(..)
  , parseEjudgeXML
  , parseEjudgeXMLs
  )
where

import           Control.Exception          (Exception, throw)
import           Control.Monad              (unless)
import           Control.Monad.State.Strict (State, execState, get, modify, put)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.Function              (on)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (isNothing)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Encoding         (decodeUtf8)
import           Data.Text.Read             (decimal, signed)
import           Data.Time                  (UTCTime, addUTCTime, defaultTimeLocale, parseTimeM)
import           EjStand.BaseModels
import           EjStand.InternalsCore
import           EjStand.StandingModels     (StandingSource (..))
import qualified Xeno.SAX                   as Xeno

-- Text casts

mconcat' :: [Text] -> String
mconcat' = Text.unpack . mconcat

wrap1BS :: (Text -> v) -> ByteString -> v
wrap1BS f = f . decodeUtf8

wrap2BS :: (Text -> Text -> v) -> ByteString -> ByteString -> v
wrap2BS f = f `on` decodeUtf8

-- Exceptions

data ParsingException = DuplicateKey         !Text
                      | MissingKey           !Text
                      | InvalidInteger       !Text
                      | InvalidRunStatus     !Text
                      | InvalidContestNumber !Int
                      | RunsInNotStartedContest

instance Exception ParsingException

instance Show ParsingException where
  show (DuplicateKey key)       = mconcat' ["Duplicate value for key \"", key, "\" for the same tag"]
  show (MissingKey key)         = mconcat' ["Key \"", key, "\" expected, but not found"]
  show (InvalidInteger str)     = mconcat' ["Can't convert string value \"", str, "\" to integer"]
  show (InvalidContestNumber n) = concat ["There must be only one contest in XML file, but ", show n, " got"]
  show (InvalidRunStatus str)   = mconcat' ["Can't convert string \"", str, "\" to run status"]
  show RunsInNotStartedContest  = "There are runs in a contest which has not started yet"

-- Map operations

(!?) :: Ord k => k -> Map k v -> Maybe v
(!?) = Map.lookup

(!) :: Text -> Map Text v -> v
key ! mp = case key !? mp of
  Nothing      -> throw $ MissingKey key
  (Just value) -> value

-- Parsing state

data ParsingState = ParsingState { lastOpenedTag    :: !(Maybe Text)
                                 , argumentList     :: !(Map Text Text)
                                 , textContents     :: !Text
                                 , stateContests    :: ![Contest]
                                 , stateContestants :: ![Contestant]
                                 , stateProblems    :: ![Problem]
                                 , stateLanguages   :: ![Language]
                                 , stateRuns        :: ![Run]
                                 }
                    deriving (Show)

emptyPS :: ParsingState
emptyPS = ParsingState Nothing Map.empty "" [] [] [] [] []

type ParsingStateM = State ParsingState

-- Data type readers

readInteger :: Text -> Integer
readInteger str = case (signed decimal) str of
  Left  _             -> throw $ InvalidInteger str
  Right (value, tail) -> if Text.null tail then value else throw $ InvalidInteger str

readUTC :: Monad a => Text -> a UTCTime
readUTC = parseTimeM True defaultTimeLocale "%Y/%m/%d %T" . Text.unpack

runStatusReadingMap :: Map Text Int
runStatusReadingMap = Map.fromList $ fmap (\x -> (Text.pack . show $ x, fromEnum x)) (allValues :: [RunStatus])

readStatus :: Text -> RunStatus
readStatus text = case Map.lookup text runStatusReadingMap of
  Nothing       -> throw $ InvalidRunStatus text
  (Just status) -> toEnum status

-- Parsing state folding

getStateContest :: ParsingState -> Contest
getStateContest state = case stateContests state of
  [contest] -> contest
  _         -> throw . InvalidContestNumber . length . stateContests $ state

makeContestTime :: Contest -> (Integer, Integer) -> UTCTime
makeContestTime contest (sec, nsec) = makeContestTime' (contestStartTime contest) (fromIntegral sec, fromIntegral nsec)
 where
  makeContestTime' :: Maybe UTCTime -> (Double, Double) -> UTCTime
  makeContestTime' Nothing     _           = throw RunsInNotStartedContest
  makeContestTime' (Just time) (sec, nsec) = addUTCTime (realToFrac (nsec * 1e-9 + sec)) time


foldPSRunlog :: ParsingState -> ParsingState
foldPSRunlog state@ParsingState {..} =
  let contestID        = readInteger $ "contest_id" ! argumentList
      contestStartTime = "start_time" !? argumentList >>= readUTC
      contest          = Contest contestID "" contestStartTime
  in  state { stateContests = (contest : stateContests) }

foldPSContestName :: ParsingState -> ParsingState
foldPSContestName state =
  state { stateContests = (\x -> x { contestName = textContents state }) <$> stateContests state }

foldPSContestant :: ParsingState -> ParsingState
foldPSContestant state@ParsingState {..} =
  let contestantID   = readInteger $ "id" ! argumentList
      contestantName = "name" ! argumentList
      contestant     = Contestant contestantID contestantName
  in  state { stateContestants = (contestant : stateContestants) }

foldPSProblem :: ParsingState -> ParsingState
foldPSProblem state@ParsingState {..} =
  let problemContest   = contestID $ getStateContest state
      problemID        = readInteger $ "id" ! argumentList
      problemShortName = "short_name" ! argumentList
      problemLongName  = "long_name" ! argumentList
      problem          = Problem problemID problemContest problemShortName problemLongName
  in  state { stateProblems = (problem : stateProblems) }

foldPSLanguage :: ParsingState -> ParsingState
foldPSLanguage state@ParsingState {..} =
  let languageID        = readInteger $ "id" ! argumentList
      languageShortName = "short_name" ! argumentList
      languageLongName  = "long_name" ! argumentList
      language          = Language languageID languageShortName languageLongName
  in  state { stateLanguages = (language : stateLanguages) }

foldPSRun :: ParsingState -> ParsingState
foldPSRun state@ParsingState {..} =
  let contest       = getStateContest state
      runID         = readInteger $ "run_id" ! argumentList
      runContest    = contestID contest
      runContestant = readInteger $ "user_id" ! argumentList
      runProblem    = readInteger <$> "prob_id" !? argumentList
      runTime       = makeContestTime contest (readInteger $ "time" ! argumentList, readInteger $ "nsec" ! argumentList)
      runStatus     = readStatus $ "status" ! argumentList
      runLanguage   = readInteger <$> "lang_id" !? argumentList
      runScore      = fromInteger . readInteger <$> "score" !? argumentList
      runTest       = readInteger <$> "test" !? argumentList
      run           = Run runID runContest runContestant runProblem runTime runStatus runLanguage runScore runTest
  in  state { stateRuns = (run : stateRuns) }

foldPS :: ParsingState -> ParsingState
foldPS state@ParsingState { lastOpenedTag = tag, ..} = case tag of
  Nothing           -> state
  (Just "runlog"  ) -> foldPSRunlog state
  (Just "name"    ) -> foldPSContestName state
  (Just "user"    ) -> foldPSContestant state
  (Just "problem" ) -> foldPSProblem state
  (Just "language") -> foldPSLanguage state
  (Just "run"     ) -> foldPSRun state
  (Just _         ) -> state

-- Parsing

skipXenoEvent :: Monad m => e -> m ()
skipXenoEvent _ = return ()

clearTagData :: ParsingStateM ()
clearTagData = do
  state <- foldPS <$> get
  put $ state { lastOpenedTag = Nothing, argumentList = Map.empty, textContents = "" }

openTag :: Text -> ParsingStateM ()
openTag tagName = do
  tag <- lastOpenedTag <$> get
  unless (isNothing tag) clearTagData
  modify (\state -> state { lastOpenedTag = Just tagName })

closeTag :: e -> ParsingStateM ()
closeTag _ = clearTagData

textInsideTag :: Text -> ParsingStateM ()
textInsideTag text = modify (\state -> state { textContents = mconcat [textContents state, text] })

processTagAttribute :: Text -> Text -> ParsingStateM ()
processTagAttribute key value = do
  state <- get
  let onDuplicate _ _ = throw $ DuplicateKey key
  let newArgumentList = Map.insertWith onDuplicate key value $ argumentList state
  put state { argumentList = newArgumentList }

stateToStandingSource :: ParsingState -> StandingSource
stateToStandingSource ParsingState {..} = StandingSource (Set.fromList stateContests)
                                                         (Set.fromList stateContestants)
                                                         (Set.fromList stateLanguages)
                                                         (Set.fromList stateProblems)
                                                         (Set.fromList stateRuns)

processRawXML :: ByteString -> StandingSource
processRawXML raw = stateToStandingSource $ (flip execState) emptyPS $ Xeno.process (wrap1BS openTag)
                                                                                    (wrap2BS processTagAttribute)
                                                                                    skipXenoEvent
                                                                                    (wrap1BS textInsideTag)
                                                                                    closeTag
                                                                                    skipXenoEvent
                                                                                    raw

-- Main entry points

parseEjudgeXML :: FilePath -> IO StandingSource
parseEjudgeXML file = processRawXML <$> BS.readFile file

parseEjudgeXMLs :: [FilePath] -> IO StandingSource
parseEjudgeXMLs filelist = do
  sources <- sequence $ map parseEjudgeXML filelist
  return $ mconcat sources
