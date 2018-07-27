module EjStand.StandingBuilder
  ( prepareStandingSource
  )
where

import           Data.Text                      ( unpack )
import           Data.List                      ( sortOn )
import qualified Data.Set                      as Set
import           Text.Printf                    ( printf )
import           EjStand.BaseModels
import           EjStand.StandingModels
import           EjStand.DataParser

-- Preparing data IO operations

prepareStandingSource :: GlobalConfiguration -> StandingConfig -> IO StandingSource
prepareStandingSource global local =
  parseEjudgeXMLs $ map (printf (unpack $ xmlFilePattern global)) $ Set.toList $ standingContests local

-- Standing building

buildProblems :: StandingConfig -> StandingSource -> [Problem]
buildProblems cfg | elem ReversedContestOrder $ standingOptions cfg = sortOn cmp . Set.toList . problems
                  | otherwise = Set.toList . problems
  where cmp = ([negate . problemContest, problemID] <*>) . return

buildStanding :: StandingConfig -> StandingSource -> Standing
buildStanding cfg src = let problems = buildProblems cfg src in undefined
