{-# LANGUAGE FlexibleInstances #-}
module EjStand.Internals.ELang.Value
  ( FromValue(..)
  , ToValue(..)
  , Value(..)
  )
where

import qualified Data.Foldable                 as Foldable
import           Data.Map.Strict                ( Map )
import           Data.Ratio                     ( Rational )
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq
import           Data.Text                      ( Text )

-- Values 

data Value = ValueVoid
           | ValueInt !Integer
           | ValueRational !Rational
           | ValueBool !Bool
           | ValueText !Text
           | ValueList !(Seq Value)
           | ValueMap !(Map Text Value)

-- ToValue class

class ToValue a where
    toValue :: a -> Value

instance ToValue () where
  toValue _ = ValueVoid

instance ToValue Integer where
  toValue = ValueInt

instance ToValue Rational where
  toValue = ValueRational

instance ToValue Bool where
  toValue = ValueBool

instance ToValue Text where
  toValue = ValueText

instance ToValue a => ToValue (Seq a) where
  toValue = ValueList . fmap toValue

instance ToValue a => ToValue [a] where
  toValue = ValueList . Seq.fromList . fmap toValue

instance ToValue a => ToValue (Map Text a) where
  toValue = ValueMap . fmap toValue

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
  fromValue _                 = Nothing

instance FromValue Bool where
  fromValue (ValueBool v) = Just v
  fromValue _             = Nothing

instance FromValue Text where
  fromValue (ValueText v) = Just v
  fromValue _             = Nothing

instance FromValue a => FromValue (Seq a) where
  fromValue (ValueList v) = sequence $ fromValue <$> v
  fromValue _             = Nothing

instance FromValue a => FromValue [a] where
  fromValue = ((Foldable.toList :: Seq b -> [b]) <$>) . fromValue

instance FromValue a => FromValue (Map Text a) where
  fromValue (ValueMap v) = sequence $ fromValue <$> v
  fromValue _            = Nothing
