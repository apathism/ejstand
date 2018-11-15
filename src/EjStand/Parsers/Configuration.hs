{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module EjStand.Parsers.Configuration
  ( parseStandingConfig
  , parseStandingConfigDirectory
  , retrieveStandingConfigs
  , parseGlobalConfiguration
  , retrieveGlobalConfiguration
  )
where

import           Control.Exception          (Exception, IOException, catch, throw)
import           Control.Monad.State.Strict (State, evalState, get, put)
import qualified Data.ByteString            as B
import           Data.Char                  (isDigit, isLetter)
import qualified Data.Foldable              as Foldable
import qualified Data.List                  as List
import           Data.Map.Strict            (Map, insertWith, (!?))
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, fromMaybe, listToMaybe)
import           Data.Ratio                 ((%))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text, unpack)
import qualified Data.Text                  as Text
import           Data.Text.Encoding         (decodeUtf8)
import           Data.Text.Read             (decimal)
import           Data.Time                  (UTCTime, defaultTimeLocale, parseTimeM)
import           EjStand.Internals.Core
import qualified EjStand.Internals.Regex    as RE
import           EjStand.Models.Standing
import           Prelude                    hiding (toInteger)
import           System.Directory.Tree      (dirTree, readDirectoryWithL)

-- Exceptions

data ParsingException = NoValue Text
                      | UndefinedKey Text
                      | DuplicateKey Text
                      | TextValueExpected Text
                      | NestedConfigExpected Text
                      | IntegerExpected Text Text
                      | BoolExpected Text Text
                      | RationalExpected Text Text
                      | TimeExpected Text Text
                      | InvalidInterval Text
                      | InvalidCondition Text Text
                      | InvalidRegex Text Text
                      | UnexpectedKey Text

instance Exception ParsingException

instance Show ParsingException where
  show (NoValue key)                = "Value expected for key \"" ++ unpack key ++ "\", but got no = or {"
  show (UndefinedKey key)           = "No key \"" ++ unpack key ++ "\" found, but it's mandatory for a config"
  show (DuplicateKey key)           = "Key \"" ++ unpack key ++ "\" has multiple values, but must be unique"
  show (TextValueExpected key)      = "Text value at key \"" ++ unpack key ++ "\" expected, but other type of value found"
  show (NestedConfigExpected key)   = "Nested configuration at key \"" ++ unpack key ++ "\" expected, but other type of value found"
  show (IntegerExpected key value)  = "Integer expected, but \"" ++ unpack value ++ "\" got while parsing value of key \"" ++ unpack key ++ "\""
  show (BoolExpected key value)     = "Bool expected, but \"" ++ unpack value ++ "\" got while parsing value of key \"" ++ unpack key ++ "\""
  show (RationalExpected key value) = "Rational expected, but \"" ++ unpack value ++ "\" got while parsing value of key \"" ++ unpack key ++ "\""
  show (TimeExpected key value)     = "Time expected, but \"" ++ unpack value ++ "\" got while parsing value of key \"" ++ unpack key ++ "\""
  show (InvalidCondition key value) = "Condition expected, but \"" ++ unpack value ++ "\" got while parsing value of key \"" ++ unpack key ++ "\""
  show (InvalidInterval key)        = "Invalid interval on key \"" ++ unpack key ++ "\""
  show (InvalidRegex key value)     = "Unable to parse value \"" ++ unpack value ++ "\" to regular expression on key \"" ++ unpack key ++ "\""
  show (UnexpectedKey key)          = "Unexpected key \"" ++ unpack key ++ "\""

-- Function tools

(.>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
(.>) f1 f2 x = f2 <$> f1 x

(|>) :: Functor f => (a -> f b) -> (a -> b -> c) -> a -> f c
(|>) f1 f2 x = (f1 .> f2 x) x

(|.>) :: (Functor f, Functor g) => (a -> f (g b)) -> (b -> c) -> a -> f (g c)
(|.>) f1 f2 x = (f2 <$>) <$> f1 x

(||>) :: (Functor f, Functor g) => (a -> f (g b)) -> (a -> b -> c) -> a -> f (g c)
(||>) f1 f2 x = (f1 |.> f2 x) x

-- Internal representation of configuration tree

data ConfigValue = TextValue Text
                 | NestedConfig Configuration
                 deriving (Show)

type Configuration = Map Text [ConfigValue]

type ParsingState = State [Text]

-- Configuration parser

takeOneIf :: (Char -> Bool) -> Text -> (Text, Text)
takeOneIf pred str = case Text.uncons str of
  Nothing     -> ("", str)
  Just (c, t) -> if pred c then (Text.singleton c, t) else ("", str)

splitKeyValue :: Text -> (Text, Text, Text)
splitKeyValue str = (key, symbol, content)
 where
  (key   , str' ) = Text.span isLetter str
  (symbol, str'') = takeOneIf ((== '=') ||| (== '{')) $ Text.stripStart str'
  content         = Text.stripStart str''

buildConfig' :: Configuration -> ParsingState Configuration
buildConfig' cfg = do
  buffer <- get
  case buffer of
    []      -> return cfg
    (h : t) -> do
      put t
      if h == "}"
        then return cfg
        else do
          let (key, symbol, content) = splitKeyValue h
          new_elem <- case symbol of
            "=" -> return [TextValue content]
            "{" -> return . NestedConfig <$> buildConfig' Map.empty
            _   -> throw $ NoValue key
          buildConfig' $ insertWith (++) key new_elem cfg

removeComment :: Text -> Text
removeComment = fst . Text.breakOn "--"

buildConfig :: Text -> Configuration
buildConfig =
  evalState (buildConfig' Map.empty)
    . filter (not . Text.null)
    . map (Text.stripEnd . removeComment . Text.strip)
    . Text.lines

-- Traversing configuration tree

type TraversingState = State Configuration

takeValuesByKey :: Text -> TraversingState [ConfigValue]
takeValuesByKey key = do
  config <- get
  put $ Map.delete key config
  return $ fromMaybe [] $ config !? key

takeUniqueValue :: Text -> TraversingState (Maybe ConfigValue)
takeUniqueValue key = unique <$> takeValuesByKey key
 where
  unique :: [a] -> Maybe a
  unique (_ : _ : _) = throw $ DuplicateKey key
  unique list        = listToMaybe list

takeMandatoryValue :: Text -> TraversingState ConfigValue
takeMandatoryValue key = fromMaybe (throw $ UndefinedKey key) <$> takeUniqueValue key

toTextValue :: Text -> ConfigValue -> Text
toTextValue _   (TextValue txt) = txt
toTextValue key _               = throw $ TextValueExpected key

toNestedConfig :: Text -> ConfigValue -> Configuration
toNestedConfig _   (NestedConfig cfg) = cfg
toNestedConfig key _                  = throw $ NestedConfigExpected key

toInteger :: Integral a => Text -> Text -> a
toInteger key value = case decimal $ Text.strip value of
  Left  _              -> throw $ IntegerExpected key value
  Right (value', tail) -> if tail == "" then value' else throw $ IntegerExpected key value

toBool :: Text -> Text -> Bool
toBool key value = case Text.strip value of
  "1"     -> True
  "0"     -> False
  "True"  -> True
  "False" -> False
  _       -> throw $ BoolExpected key value

toRatio :: Text -> Text -> Rational
toRatio key value = case map (toInteger key) $ Text.splitOn "/" value of
  [x]    -> fromIntegral x
  [a, b] -> a % b
  _      -> throw $ RationalExpected key value

toUTC :: Text -> Text -> UTCTime
toUTC key value = case parseTimeM True defaultTimeLocale "%F %R" $ unpack value of
  (Just value) -> value
  Nothing      -> throw $ TimeExpected key value

toComparison :: Text -> Text -> Comparison Rational
toComparison key value =
  let (op, arg) = Text.break isDigit $ Text.filter (/= ' ') value
      !sign     = fromMaybe (throw $ InvalidCondition key value) (readSign op)
      !ratio    = toRatio key arg
  in  Comparison sign ratio

toComparisons :: Text -> Text -> [Comparison Rational]
toComparisons key value = case Text.splitOn "," value of
  []  -> throw $ InvalidCondition key value
  lst -> toComparison key <$> lst

toIntervalValue :: Text -> Text -> Set Integer
toIntervalValue key =
  mconcat . map (readInterval key . map (toInteger key . Text.strip) . Text.splitOn "-") . Text.splitOn ","
 where
  readInterval :: Text -> [Integer] -> Set Integer
  readInterval _   [x]    = Set.singleton x
  readInterval _   [l, r] = Set.fromDistinctAscList [l .. r]
  readInterval key _      = throw $ InvalidInterval key

toRegex :: Text -> Text -> RE.Regex
toRegex key value = case RE.buildRegex value of
  Nothing      -> throw $ InvalidRegex key value
  (Just regex) -> regex

toRegexReplacer :: Text -> Text -> RE.Replacer
toRegexReplacer _ = RE.buildReplacer

ensureEmptyState :: TraversingState ()
ensureEmptyState = do
  cfg <- get
  return $ if Map.null cfg
    then ()
    else case Map.lookupMin cfg of
      Nothing       -> ()
      Just (key, _) -> throw $ UnexpectedKey key

-- Configuration readers

buildExtraDeadline :: Configuration -> FixedDeadline
buildExtraDeadline = evalState $ do
  valueContestIDs    <- takeMandatoryValue |> toTextValue |> toIntervalValue $ "ContestIDs"
  valueContestantIDs <- takeUniqueValue ||> toTextValue ||> toIntervalValue $ "ContestantIDs"
  valueDeadline      <- takeMandatoryValue |> toTextValue |> toUTC $ "Deadline"
  !_                 <- ensureEmptyState
  return $ FixedDeadline valueContestIDs valueDeadline valueContestantIDs

buildConditionalStyle :: Configuration -> ConditionalStyle
buildConditionalStyle = evalState $ do
  styleValue <- takeMandatoryValue |> toTextValue $ "StyleValue"
  conditions <- takeMandatoryValue |> toTextValue |> toComparisons $ "Conditions"
  !_         <- ensureEmptyState
  return $ ConditionalStyle conditions styleValue

buildContestNamePattern :: Configuration -> (RE.Regex, RE.Replacer)
buildContestNamePattern = evalState $ do
  regex    <- takeMandatoryValue |> toTextValue |> toRegex $ "Pattern"
  replacer <- takeMandatoryValue |> toTextValue |> toRegexReplacer $ "Substitution"
  !_       <- ensureEmptyState
  return (regex, replacer)

buildNestedOptions :: (Configuration -> a) -> Text -> TraversingState [a]
buildNestedOptions builder optionName = do
  nested <- takeValuesByKey ||> toNestedConfig $ optionName
  return $ builder <$> nested

buildStandingConfig :: TraversingState StandingConfig
buildStandingConfig = do
  name                 <- takeMandatoryValue |> toTextValue $ "Name"
  contests             <- takeMandatoryValue |> toTextValue |> toIntervalValue $ "Contests"
  contestNamePattern   <- takeUniqueValue ||> toNestedConfig |.> buildContestNamePattern $ "ContestNamePattern"
  internalName         <- takeMandatoryValue |> toTextValue $ "InternalName"
  reversedContestOrder <- takeUniqueValue ||> toTextValue ||> toBool .> fromMaybe False $ "ReversedContestOrder"
  enableDeadlines      <- takeUniqueValue ||> toTextValue ||> toBool .> fromMaybe False $ "EnableDeadlines"
  deadlinePenalty      <- if enableDeadlines
    then takeMandatoryValue |> toTextValue |> toRatio $ "DeadlinePenalty"
    else return $ 0 % 1
  showProblemStatistics <- takeUniqueValue ||> toTextValue ||> toBool .> fromMaybe False $ "ShowProblemStatistics"
  enableScores          <- takeUniqueValue ||> toTextValue ||> toBool .> fromMaybe False $ "EnableScores"
  onlyScoreLastSubmit   <- takeUniqueValue ||> toTextValue ||> toBool .> fromMaybe False $ "OnlyScoreLastSubmit"
  showAttemptsNumber    <- takeUniqueValue ||> toTextValue ||> toBool .> fromMaybe True $ "ShowAttemptsNumber"
  showLanguages         <- takeUniqueValue ||> toTextValue ||> toBool .> fromMaybe False $ "ShowLanguages"
  fixedDeadlines        <- buildNestedOptions buildExtraDeadline "SetFixedDeadline"
  conditionalStyles     <- buildNestedOptions buildConditionalStyle "ConditionalStyle"
  !_                    <- ensureEmptyState
  return $ StandingConfig
    { standingName          = name
    , standingContests      = contests
    , contestNamePattern    = contestNamePattern
    , internalName          = internalName
    , reversedContestOrder  = reversedContestOrder
    , enableDeadlines       = enableDeadlines
    , deadlinePenalty       = deadlinePenalty
    , showProblemStatistics = showProblemStatistics
    , enableScores          = enableScores
    , onlyScoreLastSubmit   = onlyScoreLastSubmit
    , showAttemptsNumber    = showAttemptsNumber
    , fixedDeadlines        = fixedDeadlines
    , conditionalStyles     = conditionalStyles
    , showLanguages         = showLanguages
    }

parseStandingConfig :: FilePath -> IO StandingConfig
parseStandingConfig path = do
  contents <- decodeUtf8 <$> B.readFile path
  let cfg = buildConfig contents
  return $ evalState buildStandingConfig cfg

parsePossibleStandingConfigFile :: FilePath -> IO (Maybe StandingConfig)
parsePossibleStandingConfigFile path =
  if ".cfg" `List.isSuffixOf` path then Just <$> parseStandingConfig path else return Nothing

parseStandingConfigDirectory :: FilePath -> IO [StandingConfig]
parseStandingConfigDirectory path =
  catMaybes . Foldable.toList . dirTree <$> readDirectoryWithL parsePossibleStandingConfigFile path

retrieveStandingConfigs :: GlobalConfiguration -> IO [StandingConfig]
retrieveStandingConfigs = parseStandingConfigDirectory . unpack . standingConfigurationsPath

-- Global configuration

buildGlobalConfiguration :: Configuration -> GlobalConfiguration
buildGlobalConfiguration = evalState $ do
  xmlPattern   <- takeUniqueValue ||> toTextValue .> fromMaybe xmlFilePattern $ "XMLFilePattern"
  serveCfgPath <-
    takeUniqueValue ||> toTextValue .> fromMaybe ejudgeServeConfigurationsPath $ "EjudgeServeConfigurationsPath"
  standCfgPath <- takeUniqueValue ||> toTextValue .> fromMaybe standingConfigurationsPath $ "StandingConfigurationsPath"
  port         <- takeUniqueValue ||> toTextValue ||> toInteger .> fromMaybe ejStandPort $ "Port"
  hostname     <- takeUniqueValue ||> toTextValue .> fromMaybe ejStandHostname $ "Hostname"
  webroot      <- takeUniqueValue ||> toTextValue .> fromMaybe webRoot $ "WebRoot"
  !_           <- ensureEmptyState
  return $ GlobalConfiguration xmlPattern serveCfgPath standCfgPath port hostname webroot
  where GlobalConfiguration {..} = defaultGlobalConfiguration

parseGlobalConfiguration :: FilePath -> IO GlobalConfiguration
parseGlobalConfiguration path = do
  contents <- decodeUtf8 <$> B.readFile path
  let cfg = buildConfig contents
  return $ buildGlobalConfiguration cfg

retrieveGlobalConfiguration :: [FilePath] -> IO (Maybe GlobalConfiguration)
retrieveGlobalConfiguration []            = return Nothing
retrieveGlobalConfiguration (file : rest) = catch (Just <$> parseGlobalConfiguration file)
                                                  (noFileExceptionHandler rest)
 where
  noFileExceptionHandler :: [FilePath] -> IOException -> IO (Maybe GlobalConfiguration)
  noFileExceptionHandler rest _ = retrieveGlobalConfiguration rest
