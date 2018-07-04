module EjStand.StandingsModels (StandingsSource(..)) where

import           Data.Set (Set)
import qualified Data.Set           as Set
import           EjStand.BaseModels

data StandingsSource = StandingsSource { contests :: Set Contest,
                                         contestants :: Set Contestant,
                                         languages :: Set Language,
                                         problems :: Set Problem,
                                         runs :: Set Run
                                       }
                     deriving (Show)
