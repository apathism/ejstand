{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
module EjStand.BaseModels
  ( Contestant(..)
  , Contest(..)
  , Problem(..)
  , Language(..)
  , RunStatus(..)
  , Run(..)
  , IdentifiableBy(..)
  , fromIdentifiableList
  )
where

import           Data.Function   (on)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import           Data.Time       (UTCTime)

-- Identifiable typeclass and some related operations

class IdentifiableBy k a | a -> k where
  getID :: a -> k

instance {-# OVERLAPPABLE #-} (Eq k, IdentifiableBy k a) => Eq a where
  (==) = (==) `on` getID

instance {-# OVERLAPPABLE #-} (Ord k, IdentifiableBy k a) => Ord a where
  compare = compare `on` getID

fromIdentifiableList :: (Ord k, IdentifiableBy k a) => [a] -> Map k a
fromIdentifiableList lst = Map.fromList $ (\x -> (getID x, x)) <$> lst

-- Models

data Contestant = Contestant { contestantID   :: !Integer
                             , contestantName :: !Text
                             } deriving (Show)

instance IdentifiableBy Integer Contestant where
  getID = contestantID

data Contest = Contest { contestID        :: !Integer
                       , contestName      :: !Text
                       , contestStartTime :: !(Maybe UTCTime)
                       } deriving (Show)

instance IdentifiableBy Integer Contest where
  getID = contestID

data Problem = Problem { problemID        :: !Integer
                       , problemContest   :: !Integer
                       , problemShortName :: !Text
                       , problemLongName  :: !Text
                       } deriving (Show)

instance IdentifiableBy (Integer, Integer) Problem where
  getID problem = (problemContest problem, problemID problem)

data Language = Language { languageID        :: !Integer
                         , languageShortName :: !Text
                         , languageLongName  :: !Text
                         } deriving (Show)

instance IdentifiableBy Integer Language where
  getID = languageID

data RunStatus = OK | CE | RT | TL | PE | WA | CF | PT | AC | IG | DQ
               | PD | ML | SE | SV | WT | PR | RJ | SK | SY | SM | RU
               | CD | CG | AV | EM | VS | VT
               deriving (Show, Read, Eq, Bounded, Enum)

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

instance IdentifiableBy (Integer, Integer) Run where
  getID run = (runContest run, runID run)
