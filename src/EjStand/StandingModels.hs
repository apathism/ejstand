{-# LANGUAGE OverloadedStrings #-}
module EjStand.StandingModels
  ( GlobalConfiguration(..)
  , StandingSource(..)
  , StandingConfig(..)
  , StandingCell(..)
  , StandingColumn(..)
  , StandingRow(..)
  , StandingRowStats(..)
  , Standing(..)
  , RunStatusType(..)
  , FixedDeadline(..)
  , ConditionalStyle(..)
  , ComparisonSign(..)
  , Comparison(..)
  , defaultGlobalConfiguration
  , getRunStatusType
  , getStatusesByRunStatusType
  , signDisplay
  , signFunction
  , readSign
  , checkComparison
  )
where

import           Data.Map.Strict       (Map)
import           Data.Semigroup        (Semigroup, (<>))
import           Data.Set              (Set)
import           Data.String           (IsString)
import           Data.Text             (Text)
import           Data.Time             (UTCTime)
import           EjStand.BaseModels
import           EjStand.InternalsCore
import           Text.Blaze.Html       (Markup)

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
  mempty = StandingSource mempty mempty mempty mempty mempty

data GlobalConfiguration = GlobalConfiguration { xmlFilePattern             :: !Text,
                                                 standingConfigurationsPath :: !Text,
                                                 ejStandPort                :: !Int,
                                                 ejStandHostname            :: !Text,
                                                 webRoot                    :: !Text
                                               }
                                               deriving (Show)

defaultGlobalConfiguration :: GlobalConfiguration
defaultGlobalConfiguration = GlobalConfiguration
  { xmlFilePattern             = "/home/judges/%06d/var/status/dir/external.xml"
  , standingConfigurationsPath = "/etc/ejstand/cfg/"
  , ejStandPort                = 80
  , ejStandHostname            = "127.0.0.1"
  , webRoot                    = "/"
  }

data ComparisonSign = Less | LessOrEq | Greater | GreaterOrEq | Equal | NotEqual
  deriving (Show, Eq, Bounded, Enum)

data Comparison t = Comparison !ComparisonSign !t
  deriving (Show, Eq)

signDisplay :: IsString s => ComparisonSign -> s
signDisplay Less        = "<"
signDisplay LessOrEq    = "<="
signDisplay Greater     = ">"
signDisplay GreaterOrEq = ">="
signDisplay Equal       = "="
signDisplay NotEqual    = "/="

signFunction :: Ord t => ComparisonSign -> t -> t -> Bool
signFunction Less        = (<)
signFunction LessOrEq    = (<=)
signFunction Greater     = (>)
signFunction GreaterOrEq = (>=)
signFunction Equal       = (==)
signFunction NotEqual    = (/=)

readSign :: Text -> Maybe ComparisonSign
readSign text = case filter ((== text) . signDisplay) allValues of
  [value] -> Just value
  _       -> Nothing

checkComparison :: Ord t => t -> Comparison t -> Bool
checkComparison argument (Comparison sign value) = (signFunction sign) argument value

data FixedDeadline = FixedDeadline { contestIDs    :: !(Set Integer)
                                   , deadline      :: !UTCTime
                                   , contestantIDs :: !(Maybe (Set Integer))
                                   }
                                   deriving (Show)

data ConditionalStyle = ConditionalStyle { conditions :: ![Comparison Rational]
                                         , styleValue :: !Text
                                         }
                                         deriving (Show)

data StandingConfig = StandingConfig { standingName          :: !Text
                                     , standingContests      :: !(Set Integer)
                                     , internalName          :: !Text
                                     , reversedContestOrder  :: !Bool
                                     , enableDeadlines       :: !Bool
                                     , deadlinePenalty       :: !Rational
                                     , showProblemStatistics :: !Bool
                                     , enableScores          :: !Bool
                                     , onlyScoreLastSubmit   :: !Bool
                                     , showAttemptsNumber    :: !Bool
                                     , showLanguages         :: !Bool
                                     , fixedDeadlines        :: ![FixedDeadline]
                                     , conditionalStyles     :: ![ConditionalStyle]
                                     }
                      deriving (Show)

data RunStatusType =  Ignore | Mistake | Rejected | Processing | Pending | Success | Disqualified | Error
  deriving (Show, Eq, Ord, Bounded, Enum)

getStatusesByRunStatusType :: RunStatusType -> [RunStatus]
getStatusesByRunStatusType Success      = [OK]
getStatusesByRunStatusType Ignore       = [CE, IG, SK, EM, VS, VT]
getStatusesByRunStatusType Mistake      = [RT, TL, PE, WA, PT, ML, SE, WT, SY]
getStatusesByRunStatusType Error        = [CF]
getStatusesByRunStatusType Processing   = [AC, PD, RU, CD, CG, AV]
getStatusesByRunStatusType Pending      = [PR]
getStatusesByRunStatusType Rejected     = [SV, RJ, SM]
getStatusesByRunStatusType Disqualified = [DQ]

getRunStatusType :: RunStatus -> RunStatusType
getRunStatusType status = case filter (elem status . getStatusesByRunStatusType) allValues of
  [x] -> x
  _   -> error $ "Unable to find run status type for run status " ++ show status

data StandingCell = StandingCell { cellType      :: !RunStatusType
                                 , cellIsOverdue :: !Bool
                                 , cellScore     :: !Rational
                                 , cellAttempts  :: !Integer
                                 , cellMainRun   :: !(Maybe Run)
                                 }
                                 deriving (Show)

data StandingRowStats = StandingRowStats { rowSuccesses       :: !Integer
                                         , rowAttempts        :: !Integer
                                         , rowLastTimeSuccess :: !(Maybe UTCTime)
                                         , rowScore           :: !Rational
                                         }
                                         deriving (Show, Eq)

instance Semigroup StandingRowStats where
  statA <> statB = StandingRowStats { rowSuccesses = rowSuccesses statA + rowSuccesses statB,
                                      rowAttempts  = rowAttempts statA + rowAttempts statB,
                                      rowLastTimeSuccess = rowLastTimeSuccess statA `max` rowLastTimeSuccess statB,
                                      rowScore = rowScore statA + rowScore statB
                                    }

instance Monoid StandingRowStats where
  mempty = StandingRowStats 0 0 Nothing 0

data StandingRow = StandingRow { rowContestant :: !Contestant
                               , rowCells      :: !(Map (Integer, Integer) StandingCell)
                               , rowStats      :: !StandingRowStats
                               }
                               deriving (Show)

data StandingColumn = StandingColumn { columnCaption  :: !Markup
                                     , columnRowValue :: !((Integer, StandingRow) -> Markup)
                                     }

data Standing = Standing { standingConfig   :: !StandingConfig
                         , standingSource   :: !StandingSource
                         , standingProblems :: ![Problem]
                         , standingRows     :: ![StandingRow]
                         , standingColumns  :: ![StandingColumn]
                         }
