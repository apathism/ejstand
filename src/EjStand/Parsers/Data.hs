{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module EjStand.Parsers.Data
  ( ParsingException(..)
  , parseEjudgeXML
  , parseEjudgeXMLs
  )
where

import           Control.Exception              ( Exception
                                                , IOException
                                                , catch
                                                , throw
                                                )
import           Control.Monad                  ( unless )
import           Control.Monad.Trans.State.Strict
                                                ( State
                                                , execState
                                                , get
                                                , modify
                                                , put
                                                )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BSC8
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                , isNothing
                                                )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.Time                      ( UTCTime
                                                , addUTCTime
                                                , defaultTimeLocale
                                                , parseTimeM
                                                )
import           EjStand.Internals.Core         ( sconcat )
import           EjStand.Models.Base
import           EjStand.Models.Standing        ( StandingSource(..) )
import qualified Xeno.SAX                      as Xeno

-- Text casts

data ParsingException = DuplicateKey         !ByteString
                      | MissingKey           !ByteString
                      | InvalidInteger       !ByteString
                      | InvalidRunStatus     !ByteString
                      | InvalidContestNumber !Int
                      | RunsInNotStartedContest

instance Exception ParsingException

instance Show ParsingException where
  show (DuplicateKey         key) = sconcat ["Duplicate value for key \"", key, "\" for the same tag"]
  show (MissingKey           key) = sconcat ["Key \"", key, "\" expected, but not found"]
  show (InvalidInteger       str) = sconcat ["Can't convert string value \"", str, "\" to integer"]
  show (InvalidContestNumber n  ) = sconcat ["There must be only one contest in XML file, but ", show n, " got"]
  show (InvalidRunStatus     str) = sconcat ["Can't convert string \"", str, "\" to run status"]
  show RunsInNotStartedContest    = "There are runs in a contest which has not started yet"

-- Map operations

(!?) :: Ord k => k -> Map k v -> Maybe v
(!?) = Map.lookup

(!) :: ByteString -> Map ByteString v -> v
key ! mp = case key !? mp of
  Nothing      -> throw $ MissingKey key
  (Just value) -> value

-- Parsing state

data ParsingState = ParsingState { lastOpenedTag    :: !(Maybe ByteString)
                                 , argumentList     :: !(Map ByteString ByteString)
                                 , textContents     :: !ByteString
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

readInteger :: ByteString -> Integer
readInteger str = case BSC8.readInteger str of
  Just (value, tail) -> if BS.null tail then value else throw $ InvalidInteger str
  Nothing            -> throw $ InvalidInteger str

readUTC :: Monad a => ByteString -> a UTCTime
readUTC = parseTimeM True defaultTimeLocale "%Y/%m/%d %T" . BSC8.unpack

readStatus :: ByteString -> RunStatus
readStatus text = fromMaybe (throw $ InvalidRunStatus text) (readRunStatus text)

-- Parsing state folding

getStateContest :: ParsingState -> Contest
getStateContest state = case stateContests state of
  [contest] -> contest
  _         -> throw . InvalidContestNumber . length . stateContests $ state

makeContestTime :: Contest -> (Integer, Integer) -> UTCTime
makeContestTime Contest {..} (sec, nsec) = case contestStartTime of
  Nothing     -> throw RunsInNotStartedContest
  (Just time) -> addUTCTime (fromInteger sec + fromInteger nsec / fromInteger (10 ^ (9 :: Int))) time

foldPSRunlog :: ParsingState -> ParsingState
foldPSRunlog state@ParsingState {..} =
  let contestID        = readInteger $ "contest_id" ! argumentList
      contestStartTime = "start_time" !? argumentList >>= readUTC
      contest          = Contest contestID "" contestStartTime
  in  state { stateContests = contest : stateContests }

foldPSContestName :: ParsingState -> ParsingState
foldPSContestName state@ParsingState {..} =
  state { stateContests = (\x -> x { contestName = decodeUtf8 textContents }) <$> stateContests }

foldPSContestant :: ParsingState -> ParsingState
foldPSContestant state@ParsingState {..} =
  let contestantID   = readInteger $ "id" ! argumentList
      contestantName = decodeUtf8 $ "name" ! argumentList
      contestant     = Contestant contestantID contestantName
  in  state { stateContestants = contestant : stateContestants }

foldPSProblem :: ParsingState -> ParsingState
foldPSProblem state@ParsingState {..} =
  let problemContest   = contestID $ getStateContest state
      problemID        = readInteger $ "id" ! argumentList
      problemShortName = decodeUtf8 $ "short_name" ! argumentList
      problemLongName  = decodeUtf8 $ "long_name" ! argumentList
      problem          = Problem problemID problemContest problemShortName problemLongName 100 0
  in  state { stateProblems = problem : stateProblems }

foldPSLanguage :: ParsingState -> ParsingState
foldPSLanguage state@ParsingState {..} =
  let languageID        = readInteger $ "id" ! argumentList
      languageShortName = decodeUtf8 $ "short_name" ! argumentList
      languageLongName  = decodeUtf8 $ "long_name" ! argumentList
      language          = Language languageID languageShortName languageLongName
  in  state { stateLanguages = language : stateLanguages }

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
  in  state { stateRuns = run : stateRuns }

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

openTag :: ByteString -> ParsingStateM ()
openTag tagName = do
  tag <- lastOpenedTag <$> get
  unless (isNothing tag) clearTagData
  modify (\state -> state { lastOpenedTag = Just tagName })

closeTag :: e -> ParsingStateM ()
closeTag _ = clearTagData

textInsideTag :: ByteString -> ParsingStateM ()
textInsideTag text = modify (\state -> state { textContents = mconcat [textContents state, text] })

processTagAttribute :: ByteString -> ByteString -> ParsingStateM ()
processTagAttribute key value = do
  state <- get
  let onDuplicate _ _ = throw $ DuplicateKey key
  let newArgumentList = Map.insertWith onDuplicate key value $ argumentList state
  put state { argumentList = newArgumentList }

stateToStandingSource :: ParsingState -> StandingSource
stateToStandingSource ParsingState {..} = StandingSource (fromIdentifiableList stateContests)
                                                         (fromIdentifiableList stateContestants)
                                                         (fromIdentifiableList stateLanguages)
                                                         (fromIdentifiableList stateProblems)
                                                         (fromIdentifiableList stateRuns)

processRawXML :: ByteString -> StandingSource
processRawXML raw = stateToStandingSource $ flip execState emptyPS $ Xeno.process openTag
                                                                                  processTagAttribute
                                                                                  skipXenoEvent
                                                                                  textInsideTag
                                                                                  closeTag
                                                                                  skipXenoEvent
                                                                                  raw

-- Main entry points

parseEjudgeXML :: FilePath -> IO (Maybe StandingSource)
parseEjudgeXML file = catch (onlyIfStarted . processRawXML <$> BS.readFile file) handleNothing
 where
  handleNothing :: IOException -> IO (Maybe a)
  handleNothing _ = return Nothing

  onlyIfStarted :: StandingSource -> Maybe StandingSource
  onlyIfStarted src = case Map.elems $ contests src of
    [contest] -> case contestStartTime contest of
      Nothing -> Nothing
      _       -> Just src
    lst       -> throw $ InvalidContestNumber $ length lst

parseEjudgeXMLs :: [FilePath] -> IO StandingSource
parseEjudgeXMLs filelist = do
  sources <- sequence $ parseEjudgeXML <$> filelist
  return . mconcat . catMaybes $ sources
