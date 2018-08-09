{-# LANGUAGE OverloadedStrings #-}
module EjStand.StandingModels
  ( GlobalConfiguration(..)
  , StandingSource(..)
  , StandingConfig(..)
  , StandingOption(..)
  , StandingCell(..)
  , StandingRow(..)
  , Standing(..)
  , RunStatusType(..)
  , defaultGlobalConfiguration
  , getRunStatusType
  , getStatusesByRunStatusType
  )
where

import           Data.Map.Strict    (Map)
import           Data.Semigroup     (Semigroup, (<>))
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Text          (Text)
import           Data.Time          (UTCTime)
import           EjStand.BaseModels

data StandingSource = StandingSource { contests    :: !(Set Contest),
                                       contestants :: !(Set Contestant),
                                       languages   :: !(Set Language),
                                       problems    :: !(Set Problem),
                                       runs        :: !(Set Run)
                                     }
                     deriving (Show)

instance Semigroup StandingSource where
  (<>) x y = fromTuple $ (toTuple x) <> (toTuple y) where
    toTuple z = (contests z, contestants z, languages z, problems z, runs z)
    fromTuple (a, b, c, d, e) = StandingSource a b c d e

instance Monoid StandingSource where
  mempty = StandingSource (mempty) (mempty) (mempty) (mempty) (mempty)

data GlobalConfiguration = GlobalConfiguration { xmlFilePattern             :: Text,
                                                 standingConfigurationsPath :: Text
                                               }
                                               deriving (Show)

defaultGlobalConfiguration :: GlobalConfiguration
defaultGlobalConfiguration = GlobalConfiguration
  { xmlFilePattern             = "/home/judges/%06d/var/status/dir/standings.xml"
  , standingConfigurationsPath = "/etc/ejstand/cfg/"
  }

data StandingConfig = StandingConfig { standingName     :: !Text,
                                       standingContests :: !(Set Integer),
                                       internalName     :: !Text,
                                       standingOptions  :: ![StandingOption]
                                     }
                      deriving (Show)

data StandingOption = ReversedContestOrder
                    | EnableDeadlines
                    | SetFixedDeadline { contestIDs    :: !(Set Integer),
                                         deadline      :: !UTCTime,
                                         contestantIDs :: !(Maybe (Set Integer))
                                       }
                    | SetDeadlinePenalty Rational
                    | ShowProblemStatistics
                    | EnableScores
                    | OnlyScoreLastSubmit
                    | ShowLanguages
                    deriving (Show, Eq)

data RunStatusType =  Ignore | Mistake | Rejected | Processing | Pending | Success | Disqualified | Error
  deriving (Show, Eq, Ord, Bounded, Enum)

getStatusesByRunStatusType :: RunStatusType -> [RunStatus]
getStatusesByRunStatusType Success      = [OK]
getStatusesByRunStatusType Ignore       = [CE, IG, SK, EM, VS, VT]
getStatusesByRunStatusType Mistake      = [RT, TL, PE, WA, PT, ML, SE, WT, SY]
getStatusesByRunStatusType Error        = [CF]
getStatusesByRunStatusType Processing   = [AC, PD, RU, CD, CG, AV, RJ]
getStatusesByRunStatusType Pending      = [PR]
getStatusesByRunStatusType Rejected     = [SV, RJ, SM]
getStatusesByRunStatusType Disqualified = [DQ]

getRunStatusType :: RunStatus -> RunStatusType
getRunStatusType status = case filter (elem status . getStatusesByRunStatusType) [minBound .. maxBound] of
  [x] -> x
  _   -> error $ "Unable to find run status type for run status " ++ show status

data StandingCell = StandingCell { cellType      :: !RunStatusType,
                                   cellIsOverdue :: !Bool,
                                   cellScore     :: !Rational,
                                   cellAttempts  :: !Integer,
                                   cellMainRun   :: !(Maybe Run)
                                 }
                                 deriving (Show)

data StandingRow = StandingRow { rowContestant :: !Contestant,
                                 rowCells      :: !(Map (Integer, Integer) StandingCell)
                               }
                               deriving (Show)

data Standing = Standing { standingConfig   :: !StandingConfig,
                           standingSource   :: !StandingSource,
                           standingProblems :: ![Problem],
                           standingRows     :: ![StandingRow]
                         }
                         deriving (Show)
