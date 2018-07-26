module EjStand.StandingsModels
  ( StandingsSource(..)
  , StandingConfig(..)
  , StandingOption(..)
  )
where

import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Data.Semigroup                 ( Semigroup
                                                , (<>)
                                                )

import           EjStand.BaseModels

data StandingsSource = StandingsSource { contests :: Set Contest,
                                         contestants :: Set Contestant,
                                         languages :: Set Language,
                                         problems :: Set Problem,
                                         runs :: Set Run
                                       }
                     deriving (Show)

instance Semigroup StandingsSource where
  (<>) x y = fromTuple $ (toTuple x) <> (toTuple y) where
    toTuple z = (contests z, contestants z, languages z, problems z, runs z)
    fromTuple (a, b, c, d, e) = StandingsSource a b c d e

instance Monoid StandingsSource where
  mempty = StandingsSource (mempty) (mempty) (mempty) (mempty) (mempty)

data StandingConfig = StandingConfig { standingName :: !Text,
                                       standingContests :: Set Integer,
                                       standingOptions :: [StandingOption]
                                     }
                      deriving (Show)

data StandingOption = ReversedContestOrder
                    | EnableDeadlines
                    | SetFixedDeadline { contestIDs :: [Integer],
                                         deadline :: !UTCTime,
                                         contestantIDs :: Maybe [Integer]
                                       }
                    | SetDeadlinePenalty Rational
                    | ShowProblemStatistics
                    | EnableScores
                    | OnlyScoreLastSubmit
                    | ShowLanguages
                    deriving (Show)
