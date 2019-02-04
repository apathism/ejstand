{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module EjStand.Models.Base
  ( Contestant(..)
  , Contest(..)
  , Problem(..)
  , Language(..)
  , RunStatus(..)
  , Run(..)
  , filterRunMap
  , readRunStatus
  )
where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           EjStand.Internals.ADTReader    ( mkADTReader )
import           EjStand.Internals.Core         ( Identifiable(..) )

-- Models

data Contestant = Contestant { contestantID   :: !Integer
                             , contestantName :: !Text
                             } deriving (Show)

instance Identifiable Contestant where
  type Identificator Contestant = Integer
  getID = contestantID

data Contest = Contest { contestID        :: !Integer
                       , contestName      :: !Text
                       , contestStartTime :: !(Maybe UTCTime)
                       } deriving (Show)

instance Identifiable Contest where
  type Identificator Contest = Integer
  getID = contestID

data Problem = Problem { problemID         :: !Integer
                       , problemContest    :: !Integer
                       , problemShortName  :: !Text
                       , problemLongName   :: !Text
                       , problemMaxScore   :: !Integer
                       , problemRunPenalty :: !Integer
                       } deriving (Show)

instance Identifiable Problem where
  type Identificator Problem = (Integer, Integer)
  getID problem = (problemContest problem, problemID problem)

data Language = Language { languageID        :: !Integer
                         , languageShortName :: !Text
                         , languageLongName  :: !Text
                         } deriving (Show)

instance Identifiable Language where
  type Identificator Language = Integer
  getID = languageID

data RunStatus = OK | CE | RT | TL | PE | WA | CF | PT | AC | IG | DQ
               | PD | ML | SE | SV | WT | PR | RJ | SK | SY | SM | RU
               | CD | CG | AV | EM | VS | VT
               deriving (Show, Eq, Ord, Bounded, Enum)

mkADTReader ''RunStatus "readRunStatus" id

data Run = Run { runID         :: !Integer
               , runContest    :: !Integer
               , runContestant :: !Integer
               , runProblem    :: !(Maybe Integer)
               , runTime       :: !UTCTime
               , runStatus     :: !RunStatus
               , runLanguage   :: !(Maybe Integer)
               , runScore      :: !(Maybe Rational)
               , runTest       :: !(Maybe Integer)
               } deriving (Show)

instance Identifiable Run where
  type Identificator Run = (Integer, Integer, Maybe Integer, Integer)
  getID run = (runContest run, runContestant run, runProblem run, runID run)

filterRunMap :: Integer -> Integer -> Maybe Integer -> Map (Identificator Run) Run -> Map (Identificator Run) Run
filterRunMap contestID contestantID problemID = Map.takeWhileAntitone pTake . Map.dropWhileAntitone pDrop
 where
  values = (contestID, contestantID, problemID)
  pDrop (a, b, c, _) = (a, b, c) < values
  pTake (a, b, c, _) = (a, b, c) == values
