{-# LANGUAGE OverloadedStrings #-}
module EjStand.Internals.Core
  ( (==>)
  , (|||)
  , allValues
  , findExactlyOne
  , textReplaceLast
  )
where

import           Control.Applicative (Alternative (..), liftA2)
import           Data.Text           (Text, breakOnEnd, stripSuffix)

-- Predicates

(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(|||) = liftA2 (||)

-- List expressions

(==>) :: Alternative f => Bool -> a -> f a
(==>) False _ = empty
(==>) True  x = pure x

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound .. maxBound]

findExactlyOne :: (a -> Bool) -> [a] -> Maybe a
findExactlyOne p lst = case filter p lst of
  []  -> Nothing
  [x] -> Just x
  _   -> Nothing

-- Text operations

textReplaceLast :: Text -> Text -> Text -> Text
textReplaceLast needle replacement haystack = case breakOnEnd needle haystack of
  (""   , str   ) -> str
  (first, second) -> case stripSuffix needle first of
    Nothing       -> error "textReplaceLast: Unexpected behaviour during cutting off needle"
    (Just first') -> mconcat [first', replacement, second]