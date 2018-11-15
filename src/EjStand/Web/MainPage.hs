{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module EjStand.Web.MainPage
  ( renderStanding
  , renderCSS
  )
where

import           Data.Char                     (isSpace)
import           Data.Map.Strict               ((!?))
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Internal.Lazy       as LT
import           EjStand                       (getVersion)
import           EjStand.Internals.Core        ((==>))
import qualified EjStand.Internals.Regex       as RE
import           EjStand.Models.Base
import           EjStand.Models.Standing
import           EjStand.Web.HtmlElements
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet                   (ihamletFile)
import           Text.Lucius                   (luciusFile, renderCss)
import           Text.Shakespeare.I18N         (Lang)

-- Utilities for templates

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [1 ..]

buildContestList :: StandingSource -> [Problem] -> [(Maybe Contest, Int)]
buildContestList StandingSource {..} problemList = groupSame $ (contests !?) . problemContest <$> problemList

groupSame :: Eq a => [a] -> [(a, Int)]
groupSame []           = []
groupSame (fst : tail) = let (e, tail') = takeSame fst 1 $! tail in (e : (groupSame $! tail'))
 where
  takeSame :: Eq a => a -> Int -> [a] -> ((a, Int), [a])
  takeSame x k [] = ((x, k), [])
  takeSame x k lst@(e : tail) | x == e    = takeSame x (k + 1) $! tail
                              | otherwise = ((x, k), lst)

getRowCellByProblem :: StandingRow -> Problem -> (Problem, StandingCell)
getRowCellByProblem row@StandingRow {..} prob@Problem {..} = case Map.lookup (problemContest, problemID) rowCells of
  (Just cell) -> (prob, cell)
  Nothing     -> error $ "Can't find standing cell for " ++ show prob ++ " in " ++ show row

getTextVersion :: Text
getTextVersion = getVersion

getShortContestName :: StandingConfig -> Contest -> Text
getShortContestName StandingConfig {..} Contest {..} = fromMaybe (T.takeWhileEnd (not . isSpace) contestName) $ do
  (regex, replacer) <- contestNamePattern
  match             <- RE.find regex contestName
  Just $ RE.evalReplacer replacer match

-- Main entry points

renderStanding :: GlobalConfiguration -> Standing -> [Lang] -> LT.Text
renderStanding GlobalConfiguration {..} standing@Standing { standingConfig = cfg@StandingConfig {..}, ..} lang =
  let problemSuccesses = showProblemStatistics ==> renderStandingProblemSuccesses lang standing
  in  renderHtml $ $(ihamletFile "templates/main.hamlet") (translate lang) skipUrlRendering

renderCSS :: LT.Text
renderCSS = renderCss ($(luciusFile "templates/main.lucius") undefined)
