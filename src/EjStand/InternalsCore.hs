{-# LANGUAGE OverloadedStrings #-}
module EjStand.InternalsCore(
    (.>),
    (|>),
    (||>),
    (==>),
    (|||),
    takeFromSetBy,
    textReplaceLast
) where

import           Control.Applicative (liftA2)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text           (Text, breakOnEnd, stripSuffix)

-- Function tools

(.>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
(.>) f1 f2 x = f2 <$> f1 x

(|>) :: Functor f => (a -> f b) -> (a -> b -> c) -> a -> f c
(|>) f1 f2 x = (f1 .> f2 x) x

(||>) :: (Functor f, Functor g) => (a -> f (g b)) -> (a -> b -> c) -> a -> f (g c)
(||>) f1 f2 x = (f2 x <$>) <$> f1 x

-- Character predicates

(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(|||) = liftA2 (||)

-- List expressions

(==>) :: Bool -> a -> [a]
(==>) False _ = []
(==>) True  x = [x]

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
