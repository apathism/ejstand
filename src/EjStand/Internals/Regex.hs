{-# LANGUAGE OverloadedStrings #-}
module EjStand.Internals.Regex
  ( Regex
  , Match
  , Replacer
  , buildRegex
  , buildReplacer
  , find
  , findAll
  , evalReplacer
  )
where

import           Data.Char                      ( digitToInt
                                                , isHexDigit
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.ICU                  ( Match
                                                , Regex
                                                , find
                                                , findAll
                                                )
import qualified Data.Text.ICU                 as ICU
import           Data.Text.Lazy                 ( toStrict )
import           Data.Text.Lazy.Builder         ( Builder )
import qualified Data.Text.Lazy.Builder        as Builder

-- Regex

defaultMatchOptions :: [ICU.MatchOption]
defaultMatchOptions = []

buildRegex :: Text -> Maybe Regex
buildRegex source = case ICU.regex' defaultMatchOptions source of
  (Left  _    ) -> Nothing
  (Right regex) -> Just regex

-- Replacer

data ReplacerItem = RawTextReplacer !Text
                  | MatchGroup !Int

type Replacer = [ReplacerItem]

type ReplacerBuildState = (Bool, Builder, Replacer)

builderToText :: Builder -> Text
builderToText = toStrict . Builder.toLazyText

emptyReplacerBuildState :: ReplacerBuildState
emptyReplacerBuildState = (False, mempty, [])

(<>.) :: Builder -> Char -> Builder
builder <>. c = builder <> Builder.singleton c

oneCharFold :: ReplacerBuildState -> Char -> ReplacerBuildState
oneCharFold (False, builder, replacer) c | c == '$'  = (True, builder, replacer)
                                         | otherwise = (False, builder <>. c, replacer)
oneCharFold (True, builder, replacer) c
  | c == '$'     = (False, builder <>. '$', replacer)
  | isHexDigit c = (False, mempty, MatchGroup (digitToInt c) : RawTextReplacer (builderToText builder) : replacer)
  | otherwise    = (False, builder <>. '$' <>. c, replacer)

buildReplacer :: Text -> Replacer
buildReplacer text =
  let (groupOpened, builder, preReplacer) = Text.foldl' oneCharFold emptyReplacerBuildState text
      remainder = RawTextReplacer . builderToText $ if groupOpened then builder <>. '$' else builder
  in  reverse (remainder : preReplacer)

evalReplacerItem :: Match -> ReplacerItem -> Text
evalReplacerItem _     (RawTextReplacer text) = text
evalReplacerItem match (MatchGroup      n   ) = fromMaybe "" $ ICU.group n match

evalReplacer :: Replacer -> Match -> Text
evalReplacer replacer match = Text.concat $ evalReplacerItem match <$> replacer
