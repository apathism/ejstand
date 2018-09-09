{-# LANGUAGE OverloadedStrings #-}
module EjStand.InternalsCore
  ( (==>)
  , (|||)
  , allValues
  , takeFromSetBy
  , textReplaceLast
  )
where

import           Control.Applicative (liftA2)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text           (Text, breakOnEnd, stripSuffix)

-- Predicates

(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(|||) = liftA2 (||)

-- List expressions

(==>) :: Bool -> a -> [a]
(==>) False _ = []
(==>) True  x = [x]

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound .. maxBound]

-- Set operations

takeFromSetBy :: Ord b => (a -> b) -> b -> Set a -> Set a
takeFromSetBy f x = Set.takeWhileAntitone ((== x) . f) . Set.dropWhileAntitone ((< x) . f)

-- Text operations

textReplaceLast :: Text -> Text -> Text -> Text
textReplaceLast needle replacement haystack = case breakOnEnd needle haystack of
  (""   , str   ) -> str
  (first, second) -> case stripSuffix needle first of
    Nothing       -> error "textReplaceLast: Unexpected behaviour during cutting off needle"
    (Just first') -> mconcat [first', replacement, second]
