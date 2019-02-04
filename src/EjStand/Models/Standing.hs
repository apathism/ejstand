{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
module EjStand.Models.Standing
  ( GlobalConfiguration(..)
  , StandingSource(..)
  , StandingConfig(..)
  , StandingCell(..)
  , StandingColumn(..)
  , GenericStandingColumn(..)
  , StandingRow(..)
  , StandingRowStats(..)
  , Standing(..)
  , RunStatusType(..)
  , FixedDeadline(..)
  , ConditionalStyle(..)
  , ColumnVariant(..)
  , OrderType(..)
  , defaultGlobalConfiguration
  , getRunStatusType
  , allColumnVariants
  , readColumnVariant
  )
where

import           Data.Maybe                     ( fromJust )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Semigroup                 ( Semigroup
                                                , (<>)
                                                )
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Time                      ( UTCTime )
import qualified EjStand.ELang                 as ELang
import           EjStand.Internals.ADTReader    ( mkADTReader
                                                , mkADTReaderList
                                                )
import           EjStand.Internals.Core
import qualified EjStand.Internals.Regex       as RE
import           EjStand.Models.Base
import           Text.Blaze.Html                ( Markup
                                                , (!)
                                                , preEscapedTextValue
                                                )
import           Text.Blaze.Html5               ( td
                                                , th
                                                )
import           Text.Blaze.Html5.Attributes    ( class_
                                                , rowspan
                                                , title
                                                )

data StandingSource = StandingSource { contests    :: !(Map Integer Contest)
                                     , contestants :: !(Map Integer Contestant)
                                     , languages   :: !(Map Integer Language)
                                     , problems    :: !(Map (Integer, Integer) Problem)
                                     , runs        :: !(Map (Identificator Run) Run)
                                     }
                     deriving (Show)

instance Semigroup StandingSource where
  (<>) x y = fromTuple $ toTuple x <> toTuple y   where
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
defaultGlobalConfiguration = GlobalConfiguration { xmlFilePattern = "/home/judges/%06d/var/status/dir/external.xml"
                                                 , ejudgeServeConfigurationsPath = "/home/judges/%06d/conf/serve.cfg"
                                                 , standingConfigurationsPath = "/etc/ejstand/cfg/"
                                                 , ejStandPort = 80
                                                 , ejStandHostname = "127.0.0.1"
                                                 , webRoot = "/"
                                                 }

data FixedDeadline = FixedDeadline { contestIDs    :: !(Set Integer)
                                   , deadline      :: !UTCTime
                                   , contestantIDs :: !(Maybe (Set Integer))
                                   }
                                   deriving (Show)

data ConditionalStyle = ConditionalStyle { conditions :: !ELang.ASTElement
                                         , styleValue :: !Text
                                         }
                                         deriving (Show)

data ColumnVariant = PlaceColumnVariant
                   | UserIDColumnVariant
                   | NameColumnVariant
                   | SuccessesColumnVariant
                   | AttemptsColumnVariant
                   | ScoreColumnVariant
                   | LastSuccessTimeColumnVariant
                   deriving (Show, Eq, Ord, Bounded, Enum)

data OrderType = Ascending | Descending
                 deriving (Show, Eq, Bounded, Enum)

mkADTReader ''ColumnVariant "readColumnVariant" (Text.unpack . fromJust . Text.stripSuffix "ColumnVariant" . Text.pack)
mkADTReaderList ''ColumnVariant "allColumnVariants" (Text.unpack . fromJust . Text.stripSuffix "ColumnVariant" . Text.pack)

data StandingConfig = StandingConfig { standingName           :: !Text
                                     , standingContests       :: !(Set Integer)
                                     , internalName           :: !Text
                                     , contestNamePattern     :: !(Maybe (RE.Regex, RE.Replacer))
                                     , reversedContestOrder   :: !Bool
                                     , displayedColumns       :: ![ColumnVariant]
                                     , mergeContestantsByName :: !Bool
                                     , rowSortingOrder        :: ![(OrderType, ColumnVariant)]
                                     , disableDefaultCSS      :: !Bool
                                     , headerAppendix         :: !(Maybe Text)
                                     , conditionalStyles      :: !(Map ColumnVariant [ConditionalStyle])
                                     , enableDeadlines        :: !Bool
                                     , deadlinePenalty        :: !Rational
                                     , fixedDeadlines         :: ![FixedDeadline]
                                     , enableScores           :: !Bool
                                     , onlyScoreLastSubmit    :: !Bool
                                     , showAttemptsNumber     :: !Bool
                                     , showSuccessTime        :: !Bool
                                     , showLanguages          :: !Bool
                                     , showProblemStatistics  :: !Bool
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
getRunStatusType = (Map.!) runStatusTypeMap

data StandingCell = StandingCell { cellType      :: !RunStatusType
                                 , cellIsOverdue :: !Bool
                                 , cellScore     :: !Rational
                                 , cellAttempts  :: !Integer
                                 , cellMainRun   :: !(Maybe Run)
                                 , cellStartTime :: !UTCTime
                                 }
                                 deriving (Show)

data StandingRowStats = StandingRowStats { rowSuccesses       :: !Integer
                                         , rowAttempts        :: !Integer
                                         , rowLastTimeSuccess :: !(Maybe UTCTime)
                                         , rowScore           :: !Rational
                                         }
                                         deriving (Show, Eq)

instance Semigroup StandingRowStats where
  statA <> statB = StandingRowStats { rowSuccesses       = rowSuccesses statA + rowSuccesses statB
                                    , rowAttempts        = rowAttempts statA + rowAttempts statB
                                    , rowLastTimeSuccess = rowLastTimeSuccess statA `max` rowLastTimeSuccess statB
                                    , rowScore           = rowScore statA + rowScore statB
                                    }

instance Monoid StandingRowStats where
  mempty = StandingRowStats 0 0 Nothing 0

data StandingRow = StandingRow { rowContestant :: !Contestant
                               , rowCells      :: !(Map (Integer, Integer) StandingCell)
                               , rowStats      :: !StandingRowStats
                               }
                               deriving (Show)

class StandingColumn c where
  type StandingColumnValue c :: *
  columnTagClass :: c -> Text
  columnCaptionText :: c -> Markup
  columnValue :: c -> Integer -> StandingRow -> StandingColumnValue c
  columnOrder :: c -> StandingRow -> StandingRow -> Ordering
  columnValueDisplayer :: c -> StandingColumnValue c -> Markup

  columnMaxValue :: Maybe (c -> StandingColumnValue c)
  columnMaxValue = Nothing

  columnCaptionTitleText :: c -> Maybe Text
  columnCaptionTitleText = const Nothing

  columnCaptionTag :: c -> Markup -> Markup
  columnCaptionTag column = let base = th ! rowspan "2" ! class_ (preEscapedTextValue . columnTagClass $ column)
                            in  case columnCaptionTitleText column of
                              Nothing -> base
                              Just t  -> base ! title (preEscapedTextValue t)

  columnValueTag :: c -> Markup -> Markup
  columnValueTag column = td ! class_ (preEscapedTextValue . columnTagClass $ column)

  columnCaption :: c -> Markup
  columnCaption column = columnCaptionTag column . columnCaptionText $ column

  columnValueCell :: c -> Integer -> StandingRow -> Markup
  columnValueCell column place row = columnValueTag column . columnValueDisplayer column $ columnValue column place row

data GenericStandingColumn = forall c . (StandingColumn c, ELang.ToValue (StandingColumnValue c)) => GenericStandingColumn c

data Standing = Standing { standingConfig   :: !StandingConfig
                         , standingSource   :: !StandingSource
                         , standingProblems :: ![Problem]
                         , standingRows     :: ![StandingRow]
                         , standingColumns  :: ![GenericStandingColumn]
                         }
