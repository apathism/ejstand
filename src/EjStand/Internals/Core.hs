{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module EjStand.Internals.Core
  ( (==>)
  , (|||)
  , allValues
  , sconcat
  , textReplaceLast
  , toString
  )
where

import           Control.Applicative            ( Alternative(..)
                                                , liftA2
                                                )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as BSC8
import           Data.Text                      ( Text
                                                , breakOnEnd
                                                , stripSuffix
                                                )
import qualified Data.Text                     as Text

-- Predicates

(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(|||) = liftA2 (||)

-- List expressions

(==>) :: Alternative f => Bool -> a -> f a
(==>) False _ = empty
(==>) True  x = pure x

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound .. maxBound]

-- Text operations

textReplaceLast :: Text -> Text -> Text -> Text
textReplaceLast needle replacement haystack = case breakOnEnd needle haystack of
  (""   , str   ) -> str
  (first, second) -> case stripSuffix needle first of
    Nothing       -> error "textReplaceLast: Unexpected behaviour during cutting off needle"
    (Just first') -> mconcat [first', replacement, second]

-- ToString typeclass

class ToString s where
  toString :: s -> String

instance ToString [Char] where
  toString = id

instance ToString Text where
  toString = Text.unpack

instance ToString ByteString where
  toString = BSC8.unpack

sconcat :: ToString s => [s] -> String
sconcat = mconcat . (toString <$>)
