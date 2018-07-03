module Ejudge.Standings.StandingsModels (StandingsSource(..)) where

import Data.List.Ordered (nubSort)
import Ejudge.Standings.BaseModels

(|++|) :: Ord a => [a] -> [a] -> [a]
(|++|) = curry $ nubSort . uncurry (++)

data StandingsSource = StandingsSource { contests :: [Contest],
                                         contestants :: [Contestant],
                                         languages :: [Language],
                                         problems :: [Problem],
                                         runs :: [Run]
                                       }
                     deriving (Show)
