module EjStand.BaseModels
  ( Contestant(..)
  , Contest(..)
  , Problem(..)
  , Language(..)
  , RunStatus(..)
  , Run(..)
  )
where

import           Data.Time                      ( UTCTime )
import           Data.Text                      ( Text )
import           Data.Function                  ( on )

data Contestant = Contestant { contestantID :: !Integer,
                               contestantName :: !Text
                             }
                deriving (Show)

instance Eq Contestant where
  (==) = (==) `on` contestantID

instance Ord Contestant where
  compare = compare `on` contestantID

data Contest = Contest { contestID :: !Integer,
                         contestName :: !Text,
                         contestStartTime :: !(Maybe UTCTime)
                       }
             deriving (Show)

instance Eq Contest where
  (==) = (==) `on` contestID

instance Ord Contest where
  compare = compare `on` contestID

data Problem = Problem { problemID :: !Integer,
                         problemContest :: !Integer,
                         problemShortName :: !Text,
                         problemLongName :: !Text
                       }
             deriving (Show)

instance Eq Problem where
  (==) = (==) `on` ([problemContest, problemID] <*>) . return

instance Ord Problem where
  compare = compare `on` ([problemContest, problemID] <*>) . return

data Language = Language { languageID :: !Integer,
                           languageShortName :: !Text,
                           languageLongName :: !Text
                         }
                deriving (Show)

instance Eq Language where
  (==) = (==) `on` languageID

instance Ord Language where
  compare = compare `on` languageID

data RunStatus = OK | CE | RT | TL | PE | WA | CF | PT | AC | IG | DQ
               | PD | ML | SE | SV | WT | PR | RJ | SK | SY | SM | RU
               | CD | CG | AV | EM | VS | VT
               deriving (Show, Read, Eq, Bounded, Enum)

data Run = Run { runID :: !Integer,
                 runContest :: !Integer,
                 runContestant :: !Integer,
                 runProblem :: !(Maybe Integer),
                 runTime :: !UTCTime,
                 runStatus :: !RunStatus,
                 runLanguage :: !(Maybe Integer),
                 runScore :: !(Maybe Integer),
                 runTest :: !(Maybe Integer)
               } deriving (Show)

instance Eq Run where
  (==) = (==) `on` ([runContest, runID] <*>) . return

instance Ord Run where
  compare = compare `on` ([runContest, runID] <*>) . return