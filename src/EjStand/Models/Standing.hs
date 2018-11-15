{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module EjStand.Models.Standing
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
  , signDisplay
  , signFunction
  , readSign
  , checkComparison
  )
where

import           Data.Map.Strict         (Map, (!))
import qualified Data.Map.Strict         as Map
import           Data.Semigroup          (Semigroup, (<>))
import           Data.Set                (Set)
import           Data.String             (IsString)
import           Data.Text               (Text)
import           Data.Time               (UTCTime)
import           EjStand.Internals.Core
import qualified EjStand.Internals.Regex as RE
import           EjStand.Models.Base
import           Text.Blaze.Html         (Markup)

data StandingSource = StandingSource { contests    :: !(Map Integer Contest)
                                     , contestants :: !(Map Integer Contestant)
                                     , languages   :: !(Map Integer Language)
                                     , problems    :: !(Map (Integer, Integer) Problem)
                                     , runs        :: !(Map RunIdentification Run)
                                     }
                     deriving (Show)

instance Semigroup StandingSource where
  (<>) x y = fromTuple $ toTuple x <> toTuple y where
    toTuple z = (contests z, contestants z, languages z, problems z, runs z)
    fromTuple (a, b, c, d, e) = StandingSource a b c d e

instance Monoid StandingSource where
  mempty = StandingSource mempty mempty mempty mempty mempty

data GlobalConfiguration = GlobalConfiguration { xmlFilePattern                :: !Text
                                               , ejudgeServeConfigurationsPath :: !Text
                                               , standingConfigurationsPath    :: !Text
                                               , ejStandPort                   :: !Int
                                               , ejStandHostname               :: !Text
                                               , webRoot                       :: !Text
                                               }
                                               deriving (Show)

defaultGlobalConfiguration :: GlobalConfiguration
defaultGlobalConfiguration = GlobalConfiguration
  { xmlFilePattern                = "/home/judges/%06d/var/status/dir/external.xml"
  , ejudgeServeConfigurationsPath = "/home/judges/%06d/conf/serve.cfg"
  , standingConfigurationsPath    = "/etc/ejstand/cfg/"
  , ejStandPort                   = 80
  , ejStandHostname               = "127.0.0.1"
  , webRoot                       = "/"
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
checkComparison argument (Comparison sign value) = signFunction sign argument value

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
                                     , contestNamePattern    :: !(Maybe (RE.Regex, RE.Replacer))
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

data RunStatusType =  Ignore | Mistake | Rejected | Processing | Pending | Success | Disqualified | Error
  deriving (Show, Eq, Ord, Bounded, Enum)

runStatusTypeMap :: Map RunStatus RunStatusType
runStatusTypeMap = $( [| runStatusTypeMap' |] )
 where
  runStatusTypeMap' :: Map RunStatus RunStatusType
  runStatusTypeMap' = Map.fromList $ (\x -> (x, searchForRunStatusType x)) <$> allValues

  searchForRunStatusType :: RunStatus -> RunStatusType
  searchForRunStatusType status = fst . head $ filter (elem status . snd) runStatusTypeRelations

  runStatusTypeRelations :: [(RunStatusType, [RunStatus])]
  runStatusTypeRelations =
    [ (Success     , [OK])
    , (Ignore      , [CE, IG, SK, EM, VS, VT])
    , (Mistake     , [RT, TL, PE, WA, PT, ML, SE, WT, SY])
    , (Error       , [CF])
    , (Processing  , [AC, PD, RU, CD, CG, AV])
    , (Pending     , [PR])
    , (Rejected    , [SV, RJ, SM])
    , (Disqualified, [DQ])
    ]

getRunStatusType :: RunStatus -> RunStatusType
getRunStatusType status = runStatusTypeMap ! status

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
  statA <> statB = StandingRowStats { rowSuccesses = rowSuccesses statA + rowSuccesses statB
                                    , rowAttempts  = rowAttempts statA + rowAttempts statB
                                    , rowLastTimeSuccess = rowLastTimeSuccess statA `max` rowLastTimeSuccess statB
                                    , rowScore = rowScore statA + rowScore statB
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
