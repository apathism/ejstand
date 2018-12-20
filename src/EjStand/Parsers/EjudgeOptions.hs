{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module EjStand.Parsers.EjudgeOptions
  ( updateStandingSourceWithProblemConfigurations
  )
where

import           Control.Applicative            ( Alternative(..) )
import           Control.Exception              ( IOException
                                                , catch
                                                )
import qualified Data.ByteString               as B
import qualified Data.List                     as List
import           Data.List.Split                ( splitOn )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                , mapMaybe
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.Text.Read                 ( decimal
                                                , signed
                                                )
import           EjStand.Models.Base
import           EjStand.Models.Standing
import           Text.Printf                    ( printf )

-- Data structures for ejudge configurations

data ProblemConfiguration = ProblemConfiguration { problemIDField   :: !(Maybe Integer)
                                                 , shortName        :: !(Maybe Text)
                                                 , ancestorTaskName :: !(Maybe Text)
                                                 , maxScore         :: !(Maybe Integer)
                                                 , runPenalty       :: !(Maybe Integer)
                                                 }
                                                 deriving (Show)

defaultProblemConfiguration :: ProblemConfiguration
defaultProblemConfiguration = ProblemConfiguration Nothing Nothing Nothing Nothing Nothing

mergeAncestor :: ProblemConfiguration -> ProblemConfiguration -> ProblemConfiguration
mergeAncestor current ancestor =
  current { maxScore = maxScore current <|> maxScore ancestor, runPenalty = runPenalty current <|> runPenalty ancestor }

ancestorList :: ProblemConfiguration -> [ProblemConfiguration] -> [ProblemConfiguration]
ancestorList current lst = case filter ((== ancestorTaskName current) . shortName) lst of
  [ancestor] -> ancestor : ancestorList ancestor lst
  _          -> []

mergeAllAncestors :: ProblemConfiguration -> [ProblemConfiguration] -> ProblemConfiguration
mergeAllAncestors current lst = List.foldl' mergeAncestor current $ ancestorList current lst

updateProblemWithContestConfigurations :: [ProblemConfiguration] -> Problem -> Problem
updateProblemWithContestConfigurations lst p@Problem {..} =
  let mergedConfiguration = do
        [ownConfiguration] <- Just $ filter ((== Just problemID) . problemIDField) lst
        Just $ mergeAllAncestors ownConfiguration lst
  in  p { problemMaxScore   = fromMaybe problemMaxScore (maxScore =<< mergedConfiguration)
        , problemRunPenalty = fromMaybe problemRunPenalty (runPenalty =<< mergedConfiguration)
        }

updateProblemWithConfigurations :: Map Integer [ProblemConfiguration] -> Problem -> Problem
updateProblemWithConfigurations m p = case Map.lookup (problemContest p) m of
  Nothing    -> p
  (Just cfg) -> updateProblemWithContestConfigurations cfg p

-- Parsers

toKeyValue :: Text -> Maybe (Text, Text)
toKeyValue line = case Text.breakOn "=" line of
  (_, "") -> Nothing
  (k, v ) -> do
    v' <- Text.stripPrefix "=" v
    Just (Text.strip k, Text.strip v')

readInteger :: Text -> Maybe Integer
readInteger value = case signed decimal value of
  (Left  _     ) -> Nothing
  (Right (e, s)) -> case Text.strip s of
    "" -> Just e
    _  -> Nothing

foldProblemConfiguration :: Maybe ProblemConfiguration -> (Text, Text) -> Maybe ProblemConfiguration
foldProblemConfiguration Nothing   _     = Nothing
foldProblemConfiguration (Just pc) tuple = case tuple of
  ("id", value) -> case problemIDField pc of
    Nothing -> Just $ pc { problemIDField = readInteger value }
    _       -> Nothing
  ("short_name", value) -> case shortName pc of
    Nothing -> Just $ pc { shortName = Just value }
    _       -> Nothing
  ("super", value) -> case ancestorTaskName pc of
    Nothing -> Just $ pc { ancestorTaskName = Just value }
    _       -> Nothing
  ("full_score", value) -> case maxScore pc of
    Nothing -> Just $ pc { maxScore = readInteger value }
    _       -> Nothing
  ("run_penalty", value) -> case runPenalty pc of
    Nothing -> Just $ pc { runPenalty = readInteger value }
    _       -> Nothing
  _ -> Just pc

analyzeProblemSection :: [Text] -> Maybe ProblemConfiguration
analyzeProblemSection =
  List.foldl' foldProblemConfiguration (Just defaultProblemConfiguration) . catMaybes . (toKeyValue <$>)

getProblemConfigurations :: [Text] -> [ProblemConfiguration]
getProblemConfigurations =
  mapMaybe analyzeProblemSection . mapMaybe (List.stripPrefix ["[problem]"]) . splitOn [""] . fmap Text.strip

-- Filesystem reading

getContestServeFileName :: GlobalConfiguration -> Contest -> FilePath
getContestServeFileName GlobalConfiguration {..} Contest {..} =
  printf (Text.unpack ejudgeServeConfigurationsPath) contestID

readContestServeFile' :: FilePath -> IO [ProblemConfiguration]
readContestServeFile' path = getProblemConfigurations . Text.lines . decodeUtf8 <$> B.readFile path

readContestServeFile :: FilePath -> IO (Maybe [ProblemConfiguration])
readContestServeFile path = catch (Just <$> readContestServeFile' path) exceptionHandler
 where
  exceptionHandler :: IOException -> IO (Maybe a)
  exceptionHandler _ = return Nothing

-- Updater

catMaybeTuples :: [(a, Maybe b)] -> [(a, b)]
catMaybeTuples []                    = []
catMaybeTuples ((_, Nothing) : tail) = catMaybeTuples tail
catMaybeTuples ((a, Just b ) : tail) = (a, b) : catMaybeTuples tail

updateStandingSourceWithProblemConfigurations :: GlobalConfiguration -> StandingSource -> IO StandingSource
updateStandingSourceWithProblemConfigurations cfg ss@StandingSource {..} = do
  let contestsList = Map.elems contests
  configurations <- sequence $ readContestServeFile . getContestServeFileName cfg <$> contestsList
  let contestConfigurations = Map.fromList . catMaybeTuples $ zip (contestID <$> contestsList) configurations
  return $ ss { problems = fmap (updateProblemWithConfigurations contestConfigurations) problems }
