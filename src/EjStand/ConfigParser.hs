{-# LANGUAGE OverloadedStrings #-}
module EjStand.ConfigParser(
  parseConfig
  ) where

import           EjStand.StandingsModels    (StandingConfig(..),
                                             StandingOption(..))
import           Data.Char                  (isDigit, isLetter)
import           Data.Text.Encoding         (decodeUtf8)
import qualified Data.ByteString as B       (readFile)
import           Data.Text                  (Text, strip, stripStart, words, isPrefixOf,
                                             span, unpack, uncons, singleton, breakOn,
                                             lines, stripEnd, null, head, splitOn)
import           Data.Text.Read             (decimal)
import           Data.Set                   (Set)
import qualified Data.Set as Set            (empty, singleton, fromDistinctAscList)
import           Data.Map.Strict            (Map, insertWith, (!?), delete)
import qualified Data.Map.Strict as Map     (empty)
import           Data.Maybe                 (fromMaybe, listToMaybe)
import           Control.Exception          (Exception, throw)
import           Control.Monad.State.Strict (State, get, put, evalState)

import           Prelude hiding             (span, lines, null, head, toInteger)

-- Character types

(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(|||) f g = \x -> f x || g x

isKeyCharacter :: Char -> Bool
isKeyCharacter = isDigit ||| isLetter ||| (== '_')

-- Exceptions

data ParsingException = NoValue Text
                      | UndefinedKey Text
                      | DuplicateKey Text
                      | NotTextValue Text
                      | IntegerExpected Text Text
                      | BoolExpected Text Text
                      | InvalidInterval Text
                      | UnexpectedKey Text

instance Exception ParsingException

instance Show ParsingException where
  show (NoValue key)               = "Value expected for key \"" ++ unpack key ++ "\", but got no = or {"
  show (UndefinedKey key)          = "No key \"" ++ unpack key ++ "\" found, but it's mandatory for a config"
  show (DuplicateKey key)          = "Key \"" ++ unpack key ++ "\" has multiple values, but must be unique"
  show (NotTextValue key)          = "Text value at key \"" ++ unpack key ++ "\" expected, but other type of value found"
  show (IntegerExpected key value) = "Integer expected, but \"" ++ unpack value ++ "\" got while parsing value of key \"" ++ unpack key ++ "\""
  show (BoolExpected key value)    = "Bool expected, but \"" ++ unpack value ++ "\" got while parsing value of key \"" ++ unpack key ++ "\""
  show (InvalidInterval key)       = "Invalid interval on key \"" ++ unpack key ++ "\""
  show (UnexpectedKey key)         = "Unexpected key"

-- Internal representation of configuration tree

data ConfigValue = TextValue Text
                 | NestedConfig Configuration
                 deriving (Show)

type Configuration = Map Text [ConfigValue]

type ParsingState = State [Text]

-- Configuration parser

takeOneIf :: (Char -> Bool) -> Text -> (Text, Text)
takeOneIf pred str = case uncons str of
  Nothing     -> ("", str)
  Just (c, t) -> if pred c then (singleton c, t) else ("", str)

splitKeyValue :: Text -> (Text, Text, Text)
splitKeyValue str = (key, symbol, content)
  where
    (key, str') = span isLetter str
    (symbol, str'') = takeOneIf ((== '=') ||| (== '{')) $ stripStart str'
    content = stripStart str''

buildConfig' :: Configuration -> ParsingState Configuration
buildConfig' cfg = do
  buffer <- get
  case buffer of
    []    -> return cfg
    (h:t) -> do
      put t      
      if h == "}" then return cfg else do
        let (key, symbol, content) = splitKeyValue h
        new_elem <- case symbol of
          "=" -> return [TextValue content]
          "{" -> return . NestedConfig <$> buildConfig' Map.empty
          _   -> throw $ NoValue key
        buildConfig' $ insertWith (++) key new_elem cfg
        
removeComment :: Text -> Text
removeComment = fst . breakOn "--"

buildConfig :: Text -> Configuration
buildConfig = evalState (buildConfig' Map.empty) . filter (not . null) . map (stripEnd . removeComment . strip) . lines

-- Traversing configuration tree

type TraversingState = State Configuration 

takeValuesByKey :: Text -> TraversingState [ConfigValue]
takeValuesByKey key = do
  config <- get
  put $ delete key config
  return $ fromMaybe [] $ config !? key

takeUniqueValue :: Text -> TraversingState (Maybe ConfigValue)
takeUniqueValue key = unique <$> takeValuesByKey key
  where
    unique :: [a] -> Maybe a
    unique (_:_:_) = throw $ DuplicateKey key
    unique list    = listToMaybe list

takeMandatoryValue :: Text -> TraversingState ConfigValue
takeMandatoryValue key = fromMaybe (throw $ UndefinedKey key) <$> takeUniqueValue key

ensureNoValue :: Text -> TraversingState ()
ensureNoValue key = do
  value <- takeUniqueValue key
  case value of
    Nothing -> return ()
    _       -> throw $ UnexpectedKey key 

toTextValue :: Text -> ConfigValue -> Text
toTextValue _ (TextValue txt) = txt
toTextValue key _             = throw $ NotTextValue key

toInteger :: Text -> Text -> Integer
toInteger key value = case decimal $ strip value of
  Left _               -> throw $ IntegerExpected key value
  Right (value', tail) -> if tail == "" then value' else throw $ IntegerExpected key value

toBool :: Text -> Text -> Bool
toBool key value = case strip value of
  "1"     -> True
  "0"     -> False
  "True"  -> True
  "False" -> False
  _       -> throw $ BoolExpected key value

toIntervalValue :: Text -> Text -> Set Integer
toIntervalValue key = mconcat . map (readInterval key . map (toInteger key . strip) . splitOn "-") . splitOn ","
  where
    readInterval :: Text -> [Integer] -> Set Integer
    readInterval _   [x]    = Set.singleton x
    readInterval _   [l, r] = Set.fromDistinctAscList [l..r]
    readInterval key _      = throw $ InvalidInterval key

-- Functor utilities

(|>) :: Functor f => (a -> f b) -> (a -> b -> c) -> a -> f c
(|>) f1 f2 x = f2 x <$> f1 x

(||>) :: (Functor f, Functor g) => (a -> f (g b)) -> (a -> b -> c) -> a -> f (g c)
(||>) f1 f2 x = (f2 x <$>) <$> f1 x

skipKey :: (b -> c) -> a -> b -> c
skipKey f _ = f

-- Readers for options

buildStandingOptions :: TraversingState [StandingOption]
buildStandingOptions = do
  reversedContestOrder <- takeUniqueValue ||> toTextValue ||> toBool |> skipKey (fromMaybe False) $ "ReversedContestOrder"
  enableDeadlines <- takeUniqueValue ||> toTextValue ||> toBool |> skipKey (fromMaybe False) $ "EnableDeadlines"
  return $ mconcat [if reversedContestOrder then [ReversedContestOrder] else [],
                    if enableDeadlines then [EnableDeadlines] else []
                   ]

buildStandingConfig' :: TraversingState StandingConfig
buildStandingConfig' = do
  standName <- takeMandatoryValue |> toTextValue $ "Name"
  standContests <- takeMandatoryValue |> toTextValue |> toIntervalValue $ "Contests"
  standOptions <- buildStandingOptions
  return $ StandingConfig standName standContests standOptions

buildStandingConfig :: Configuration -> StandingConfig
buildStandingConfig = evalState buildStandingConfig'  

--parseConfig :: FilePath -> IO StaningConfig
parseConfig path = do
  contents <- decodeUtf8 <$> B.readFile path
  let cfg = buildConfig contents
  print $ buildStandingConfig cfg
