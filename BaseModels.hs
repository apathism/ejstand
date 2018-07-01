module Ejudge.Standings.BaseModels (
  Contestant(..),
  Contest(..),
  Problem(..),
  Language(..),
  RunStatus(..),
  Run(..),
  findByAttribute,
  findByAttribute0
  ) where

import Data.Time  (UTCTime)
import Data.List  (find)
import Data.Maybe (fromJust)

identify :: (b -> b -> c) -> (a -> b) -> (a -> a -> c)
identify mapFunc keyFunc = curry $ uncurry mapFunc . mapTuple keyFunc where
  mapTuple f (x, y) = (f x, f y)

findByAttribute :: Eq b => (a -> b) -> [a] -> Maybe b -> Maybe a
findByAttribute _   _  Nothing = Nothing
findByAttribute f lst (Just v) = find ((==v) . f) lst

findByAttribute0 :: Eq b => (a -> b) -> [a] -> b -> a
findByAttribute0 f lst v = fromJust $ findByAttribute f lst (Just v) 

data Contestant = Contestant { contestantID :: Integer,
                               contestantName :: String
                             }
                deriving (Show)

instance Eq Contestant where
  (==) = identify (==) contestantID

instance Ord Contestant where
  compare = identify compare contestantID
  
data Contest = Contest { contestID :: Integer,
                         contestName :: String,
                         contestStartTime :: Maybe UTCTime,
                         contestSchedStartTime :: Maybe UTCTime
                       }
             deriving (Show)

instance Eq Contest where
  (==) = identify (==) contestID

instance Ord Contest where
  compare = identify compare contestID

data Problem = Problem { problemID :: Integer,
                         problemContest :: Contest,
                         problemShortName :: String,
                         problemLongName :: String
                       }
             deriving (Show)

instance Eq Problem where
  (==) = identify (==) keyFunc where
    keyFunc x = (contestID $ problemContest x, problemID x)

instance Ord Problem where
  compare = identify compare keyFunc where
    keyFunc x = (contestID $ problemContest x, problemID x)

data Language = Language { languageID :: Integer,
                           languageShortName :: String,
                           languageLongName :: String
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

data Run = Run { runID :: Integer,
                 runContest :: Contest,
                 runContestant :: Contestant,
                 runProblem :: Maybe Problem,
                 runTime :: UTCTime,
                 runStatus :: RunStatus,
                 runLanguage :: Maybe Language,
                 runScore :: Maybe Integer,
                 runTest :: Maybe Integer
               } deriving (Show)

instance Eq Run where
  (==) = identify (==) keyFunc where
    keyFunc x = (runID x, contestID . runContest $ x)

instance Ord Run where
  compare = identify compare keyFunc where
    keyFunc x = (runID x, contestID . runContest $ x) 
