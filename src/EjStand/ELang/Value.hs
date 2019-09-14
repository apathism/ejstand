{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module EjStand.ELang.Value
  ( FromValue(..)
  , ToValue(..)
  , Value(..)
  )
where

import           Data.Foldable                  ( toList )
import           Data.List                     as List
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Ratio                     ( Rational
                                                , denominator
                                                , numerator
                                                )
import           Data.Sequence                  ( Seq )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Data.Time.Clock.System         ( systemSeconds
                                                , utcToSystemTime
                                                )

-- Values 

data Value = ValueVoid
           | ValueInt !Integer
           | ValueRational !Rational
           | ValueDouble !Double
           | ValueBool !Bool
           | ValueText !Text
           | ValueList !(Seq Value)
           | ValueMap !(Map Text Value)
           deriving (Eq)

instance Show Value where
  show ValueVoid         = "void"
  show (ValueInt      x) = show x
  show (ValueRational r) = show a <> "/" <> show b
   where
    a = numerator r
    b = denominator r
  show (ValueDouble x) = show x
  show (ValueBool b) = show b
  show (ValueText t) = show t
  show (ValueList l) = show . toList $ l
  show (ValueMap  m) = "[" <> contents <> "]"
   where
    contents = List.intercalate ", " pairs
    pairs    = displayer <$> Map.toList m
    displayer (key, value) = show key <> ": " <> show value

-- ToValue class

class ToValue a where
    toValue :: a -> Value

instance ToValue () where
  toValue () = ValueVoid

instance ToValue Integer where
  toValue = ValueInt

instance ToValue Rational where
  toValue = ValueRational

instance ToValue Double where
  toValue = ValueDouble

instance ToValue Bool where
  toValue = ValueBool

instance ToValue Text where
  toValue = ValueText

instance ToValue UTCTime where
  toValue = ValueInt . toInteger . systemSeconds . utcToSystemTime

instance ToValue a => ToValue (Maybe a) where
  toValue Nothing      = ValueVoid
  toValue (Just value) = toValue value

-- FromValue class

class FromValue a where
    fromValue :: Value -> Maybe a

instance FromValue () where
  fromValue ValueVoid = Just ()
  fromValue _         = Nothing

instance FromValue Integer where
  fromValue (ValueInt v) = Just v
  fromValue _            = Nothing

instance FromValue Rational where
  fromValue (ValueRational v) = Just v
  fromValue (ValueInt      v) = Just . fromInteger $ v
  fromValue _                 = Nothing

instance FromValue Bool where
  fromValue (ValueBool v) = Just v
  fromValue _             = Nothing

instance FromValue Text where
  fromValue (ValueText v) = Just v
  fromValue _             = Nothing

instance FromValue Double where
  fromValue (ValueDouble v) = Just v
  fromValue (ValueInt    v) = Just . fromInteger $ v
  fromValue (ValueRational v) = Just . fromRational $ v
  fromValue _ = Nothing
