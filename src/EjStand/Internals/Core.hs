{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE UndecidableInstances   #-}
module EjStand.Internals.Core
  ( IdentifiableBy(..)
  , (==>)
  , (|||)
  , allValues
  , fromIdentifiableList
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
import           Data.Function                  ( on )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
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

-- Identifiable typeclass and some related operations

class IdentifiableBy k a | a -> k where
  getID :: a -> k

instance {-# OVERLAPPABLE #-} (Eq k, IdentifiableBy k a) => Eq a where
  (==) = (==) `on` getID

instance {-# OVERLAPPABLE #-} (Ord k, IdentifiableBy k a) => Ord a where
  compare = compare `on` getID

fromIdentifiableList :: (Ord k, IdentifiableBy k a) => [a] -> Map k a
fromIdentifiableList lst = Map.fromList $ (\x -> (getID x, x)) <$> lst
