{-# LANGUAGE OverloadedStrings #-}
module EjStand.InternalsCore
  ( (==>)
  , (|||)
  , allValues
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

-- Text operations

textReplaceLast :: Text -> Text -> Text -> Text
textReplaceLast needle replacement haystack = case breakOnEnd needle haystack of
  (""   , str   ) -> str
  (first, second) -> case stripSuffix needle first of
    Nothing       -> error "textReplaceLast: Unexpected behaviour during cutting off needle"
    (Just first') -> mconcat [first', replacement, second]
