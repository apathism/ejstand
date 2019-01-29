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

import           Control.Exception              ( Exception
                                                , IOException
                                                , catch
                                                , handle
                                                , throw
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Trans.State.Strict
                                                ( State
                                                , StateT
                                                , evalState
                                                , evalStateT
                                                , get
                                                , put
                                                )
import qualified Data.ByteString               as B
import           Data.Char                      ( isLetter )
import qualified Data.Foldable                 as Foldable
import qualified Data.List                     as List
import           Data.Map.Strict                ( Map
                                                , insertWith
                                                , (!?)
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                , listToMaybe
                                                )
import           Data.Ratio                     ( (%) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text
                                                , unpack
                                                )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.Text.Read                 ( decimal )
import           Data.Time                      ( UTCTime
                                                , defaultTimeLocale
                                                , parseTimeM
                                                )
import           EjStand.Internals.Core
import qualified EjStand.Internals.ELang       as ELang
import qualified EjStand.Internals.Regex       as RE
import           EjStand.Models.Standing
import           Prelude                 hiding ( toInteger )
import           System.Directory.Tree          ( dirTree
                                                , readDirectoryWithL
                                                )

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
                      | InvalidColumnName Text Text
                      | FileNotFound Text Text
                      | UnexpectedKey Text
                      | ELangError Text Text

instance Exception ParsingException

instance Show ParsingException where
  show (NoValue           key) = sconcat ["Value expected for key \"", key, "\", but got no = or {"]
  show (UndefinedKey      key) = sconcat ["No key \"", key, "\" found, but it's mandatory for a config"]
  show (DuplicateKey      key) = sconcat ["Key \"", key, "\" has multiple values, but must be unique"]
  show (TextValueExpected key) = sconcat ["Text value at key \"", key, "\" expected, but other type of value found"]
  show (NestedConfigExpected key) =
    sconcat ["Nested configuration at key \"", key, "\" expected, but other type of value found"]
  show (IntegerExpected key value) =
    sconcat ["Integer expected, but \"", value, "\" got while parsing value of key \"", key, "\""]
  show (BoolExpected key value) =
    sconcat ["Bool expected, but \"", value, "\" got while parsing value of key \"", key, "\""]
  show (RationalExpected key value) =
    sconcat ["Rational expected, but \"", value, "\" got while parsing value of key \"", key, "\""]
  show (TimeExpected key value) =
    sconcat ["Time expected, but \"", value, "\" got while parsing value of key \"", key, "\""]
  show (InvalidCondition key value) =
    sconcat ["Condition expected, but \"", value, "\" got while parsing value of key \"", key, "\""]
  show (InvalidInterval key) = sconcat ["Invalid interval on key \"", key, "\""]
  show (InvalidRegex key value) =
    sconcat ["Unable to parse value \"", value, "\" to regular expression on key \"", key, "\""]
  show (InvalidColumnName key col) = sconcat ["Unknown column name \"", col, "\" on key \"", key, "\""]
  show (FileNotFound key filename) =
    sconcat ["File \"", filename, "\" was mentioned in key \"", key, "\" value, but can't be found or unreadable"]
  show (UnexpectedKey key      ) = sconcat ["Unexpected key \"", key, "\""]
  show (ELangError key errorMsg) = sconcat ["ELang error occured on key \"", key, "\": ", errorMsg]

-- Function tools

(.>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
(.>) f1 f2 x = f2 <$> f1 x

(|>) :: Functor f => (a -> f b) -> (a -> b -> c) -> a -> f c
(|>) f1 f2 x = (f1 .> f2 x) x

(|.>) :: (Functor f, Functor g) => (a -> f (g b)) -> (b -> c) -> a -> f (g c)
(|.>) f1 f2 x = (f2 <$>) <$> f1 x

(||>) :: (Functor f, Functor g) => (a -> f (g b)) -> (a -> b -> c) -> a -> f (g c)
(||>) f1 f2 x = (f1 |.> f2 x) x

(||=>) :: (Traversable t, Monad m) => (a -> m (t b)) -> (a -> b -> m c) -> a -> m (t c)
(||=>) f1 f2 x = f1 x >>= (sequence . (f2 x <$>))

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

type TraversingState = StateT Configuration

takeValuesByKey :: Monad m => Text -> TraversingState m [ConfigValue]
takeValuesByKey key = do
  config <- get
  put $ Map.delete key config
  return $ fromMaybe [] $ config !? key

takeUniqueValue :: Monad m => Text -> TraversingState m (Maybe ConfigValue)
takeUniqueValue key = unique <$> takeValuesByKey key
 where
  unique :: [a] -> Maybe a
  unique (_ : _ : _) = throw $ DuplicateKey key
  unique list        = listToMaybe list

takeMandatoryValue :: Monad m => Text -> TraversingState m ConfigValue
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
toUTC key value = case parseTimeM True defaultTimeLocale "%F %T" $ unpack value of
  (Just value) -> value
  Nothing      -> throw $ TimeExpected key value

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

toELangAST :: Text -> Text -> ELang.ASTElement
toELangAST key value = case ELang.buildAST value of
  Left  errorMsg -> throw $ ELangError key errorMsg
  Right ast      -> ast

toColumnVariant :: Text -> Text -> ColumnVariant
toColumnVariant key value =
  let value' = Text.strip value
  in  case readColumnVariant value' of
        Nothing   -> throw $ InvalidColumnName key value'
        (Just cv) -> cv

toColumnVariantL :: Text -> Text -> [ColumnVariant]
toColumnVariantL key value = toColumnVariant key <$> Text.splitOn "," value

toRowSortingOrderL :: Text -> Text -> [(OrderType, ColumnVariant)]
toRowSortingOrderL key value = toRowSortingOrder key . Text.strip <$> Text.splitOn "," value
 where
  cutOrderSuffix :: Text -> Maybe (OrderType, Text)
  cutOrderSuffix text = case "[v]" `Text.stripSuffix` text of
    (Just p) -> Just (Ascending, p)
    Nothing  -> case "[^]" `Text.stripSuffix` text of
      (Just p) -> Just (Descending, p)
      Nothing  -> Nothing

  toRowSortingOrder :: Text -> Text -> (OrderType, ColumnVariant)
  toRowSortingOrder key value =
    let (order, value') = fromMaybe (Ascending, value) $ cutOrderSuffix value in (order, toColumnVariant key value')

toFileContents :: Text -> Text -> TraversingState IO Text
toFileContents key filename = liftIO $ handle handler $ decodeUtf8 <$> B.readFile (Text.unpack filename)
 where
  handler :: IOException -> IO a
  handler _ = throw $ FileNotFound key filename

transformHomePath :: Text -> Text -> Text -> Text
transformHomePath cfgpath _ path = if "/" `Text.isPrefixOf` path
  then path
  else
    let (base, _) = Text.breakOnEnd "/" cfgpath
        base'     = if "/" `Text.isSuffixOf` base then base else "./"
    in  base' <> path

ensureEmptyState :: Monad m => StateT Configuration m ()
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
  conditions <- takeMandatoryValue |> toTextValue |> toELangAST $ "Conditions"
  !_         <- ensureEmptyState
  return $ ConditionalStyle conditions styleValue

buildContestNamePattern :: Configuration -> (RE.Regex, RE.Replacer)
buildContestNamePattern = evalState $ do
  regex    <- takeMandatoryValue |> toTextValue |> toRegex $ "Pattern"
  replacer <- takeMandatoryValue |> toTextValue |> toRegexReplacer $ "Substitution"
  !_       <- ensureEmptyState
  return (regex, replacer)

buildNestedOptions :: (Configuration -> a) -> Text -> TraversingState IO [a]
buildNestedOptions builder optionName = do
  nested <- takeValuesByKey ||> toNestedConfig $ optionName
  return $ builder <$> nested

defaultDisplayedColumns :: [ColumnVariant]
defaultDisplayedColumns = [PlaceColumnVariant, NameColumnVariant, ScoreColumnVariant]

defaultSortingOrder :: [(OrderType, ColumnVariant)]
defaultSortingOrder = [(Descending, ScoreColumnVariant), (Ascending, NameColumnVariant)]

buildStandingConfig :: Text -> TraversingState IO StandingConfig
buildStandingConfig path = do
  standingName           <- takeMandatoryValue |> toTextValue $ "Name"
  standingContests       <- takeMandatoryValue |> toTextValue |> toIntervalValue $ "Contests"
  internalName           <- takeMandatoryValue |> toTextValue $ "InternalName"
  contestNamePattern     <- takeUniqueValue ||> toNestedConfig |.> buildContestNamePattern $ "ContestNamePattern"
  reversedContestOrder   <- takeUniqueValue ||> toTextValue ||> toBool .> fromMaybe False $ "ReversedContestOrder"
  displayedColumns       <-
    takeUniqueValue ||> toTextValue ||> toColumnVariantL .> fromMaybe defaultDisplayedColumns $ "DisplayedColumns"
  mergeContestantsByName <- takeUniqueValue ||> toTextValue ||> toBool .> fromMaybe False $ "MergeContestantsByName"
  rowSortingOrder        <-
    takeUniqueValue ||> toTextValue ||> toRowSortingOrderL .> fromMaybe defaultSortingOrder $ "RowSortingOrder"
  headerAppendix         <-
    takeUniqueValue ||> toTextValue ||> transformHomePath path ||=> toFileContents $ "HeaderAppendixFile"
  disableDefaultCSS      <- takeUniqueValue ||> toTextValue ||> toBool .> fromMaybe False $ "DisableDefaultCSS"
  conditionalStyles      <- buildNestedOptions buildConditionalStyle "ConditionalStyle"
  enableDeadlines        <- takeUniqueValue ||> toTextValue ||> toBool .> fromMaybe False $ "EnableDeadlines"
  deadlinePenalty        <- if enableDeadlines
    then takeMandatoryValue |> toTextValue |> toRatio $ "DeadlinePenalty"
    else return 0
  fixedDeadlines         <- buildNestedOptions buildExtraDeadline "SetFixedDeadline"
  enableScores           <- takeUniqueValue ||> toTextValue ||> toBool .> fromMaybe False $ "EnableScores"
  onlyScoreLastSubmit    <- takeUniqueValue ||> toTextValue ||> toBool .> fromMaybe False $ "OnlyScoreLastSubmit"
  showAttemptsNumber     <- takeUniqueValue ||> toTextValue ||> toBool .> fromMaybe True $ "ShowAttemptsNumber"
  showSuccessTime        <- takeUniqueValue ||> toTextValue ||> toBool .> fromMaybe False $ "ShowSuccessTime"
  showLanguages          <- takeUniqueValue ||> toTextValue ||> toBool .> fromMaybe False $ "ShowLanguages"
  showProblemStatistics  <- takeUniqueValue ||> toTextValue ||> toBool .> fromMaybe False $ "ShowProblemStatistics"
  !_                     <- ensureEmptyState
  return $ StandingConfig { standingName           = standingName
                          , standingContests       = standingContests
                          , internalName           = internalName
                          , contestNamePattern     = contestNamePattern
                          , reversedContestOrder   = reversedContestOrder
                          , displayedColumns       = displayedColumns
                          , mergeContestantsByName = mergeContestantsByName
                          , rowSortingOrder        = rowSortingOrder
                          , headerAppendix         = headerAppendix
                          , disableDefaultCSS      = disableDefaultCSS
                          , conditionalStyles      = conditionalStyles
                          , enableDeadlines        = enableDeadlines
                          , deadlinePenalty        = deadlinePenalty
                          , fixedDeadlines         = fixedDeadlines
                          , enableScores           = enableScores
                          , onlyScoreLastSubmit    = onlyScoreLastSubmit
                          , showAttemptsNumber     = showAttemptsNumber
                          , showSuccessTime        = showSuccessTime
                          , showLanguages          = showLanguages
                          , showProblemStatistics  = showProblemStatistics
                          }

parseStandingConfig :: FilePath -> IO StandingConfig
parseStandingConfig path = do
  contents <- decodeUtf8 <$> B.readFile path
  let cfg = buildConfig contents
  evalStateT (buildStandingConfig (Text.pack path)) cfg

parsePossibleStandingConfigFile :: FilePath -> IO (Maybe StandingConfig)
parsePossibleStandingConfigFile path =
  if ".cfg" `List.isSuffixOf` path then Just <$> parseStandingConfig path else return Nothing

parseStandingConfigDirectory :: FilePath -> IO [StandingConfig]
parseStandingConfigDirectory path =
  catMaybes . Foldable.toList . dirTree <$> readDirectoryWithL parsePossibleStandingConfigFile path

retrieveStandingConfigs :: GlobalConfiguration -> IO [StandingConfig]
retrieveStandingConfigs = parseStandingConfigDirectory . unpack . standingConfigurationsPath

-- Global configuration

buildGlobalConfiguration :: Configuration -> IO GlobalConfiguration
buildGlobalConfiguration = evalStateT $ do
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
  buildGlobalConfiguration cfg

retrieveGlobalConfiguration :: [FilePath] -> IO (Maybe GlobalConfiguration)
retrieveGlobalConfiguration []            = return Nothing
retrieveGlobalConfiguration (file : rest) = catch (Just <$> parseGlobalConfiguration file)
                                                  (noFileExceptionHandler rest)
 where
  noFileExceptionHandler :: [FilePath] -> IOException -> IO (Maybe GlobalConfiguration)
  noFileExceptionHandler rest _ = retrieveGlobalConfiguration rest
