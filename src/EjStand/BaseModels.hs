module EjStand.BaseModels (
  Contestant(..),
  Contest(..),
  Problem(..),
  Language(..),
  RunStatus(..),
  Run(..),
  ) where

import Data.Time (UTCTime)
import Data.Text (Text)

identify :: (b -> b -> c) -> (a -> b) -> (a -> a -> c)
identify mapFunc keyFunc = curry $ uncurry mapFunc . mapTuple keyFunc where
  mapTuple f (x, y) = (f x, f y)

data Contestant = Contestant { contestantID :: !Integer,
                               contestantName :: !Text
                             }
                deriving (Show)

instance Eq Contestant where
  (==) = identify (==) contestantID

instance Ord Contestant where
  compare = identify compare contestantID
  
data Contest = Contest { contestID :: !Integer,
                         contestName :: !Text,
                         contestStartTime :: Maybe UTCTime
                       }
             deriving (Show)

instance Eq Contest where
  (==) = identify (==) contestID

instance Ord Contest where
  compare = identify compare contestID

data Problem = Problem { problemID :: !Integer,
                         problemContest :: !Integer,
                         problemShortName :: !Text,
                         problemLongName :: !Text
                       }
             deriving (Show)

instance Eq Problem where
  (==) = identify (==) keyFunc where
    keyFunc x = (problemContest x, problemID x)

instance Ord Problem where
  compare = identify compare keyFunc where
    keyFunc x = (problemContest x, problemID x)

data Language = Language { languageID :: !Integer,
                           languageShortName :: !Text,
                           languageLongName :: !Text
                         }
                deriving (Show)

instance Eq Language where
  (==) = identify (==) languageID

instance Ord Language where
  compare = identify compare languageID

data RunStatus = OK | CE | RT | TL | PE | WA | CF | PT | AC | IG | DQ
               | PD | ML | SE | SV | WT | PR | RJ | SK | SY | SM | RU
               | CD | CG | AV | EM | VS | VT
               deriving (Show, Read, Eq, Bounded, Enum)

data Run = Run { runID :: !Integer,
                 runContest :: !Integer,
                 runContestant :: !Integer,
                 runProblem :: Maybe Integer,
                 runTime :: !UTCTime,
                 runStatus :: !RunStatus,
                 runLanguage :: Maybe Integer,
                 runScore :: Maybe Integer,
                 runTest :: Maybe Integer
               } deriving (Show)

instance Eq Run where
  (==) = identify (==) keyFunc where
    keyFunc x = (runContest x, runID x)

instance Ord Run where
  compare = identify compare keyFunc where
    keyFunc x = (runContest x, runID x) 
