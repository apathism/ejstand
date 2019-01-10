{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module EjStand.Internals.ELang.Value
  ( FromValue(..)
  , ToValue(..)
  , Value(..)
  , displayValueType
  , displayValuesType
  )
where

import           Data.Map.Strict                ( Map )
import           Data.Ratio                     ( Rational )
import           Data.Sequence                  ( Seq )
import           Data.Text                      ( Text )
import           Data.Text                     as Text

-- Values 

data Value = ValueVoid
           | ValueInt !Integer
           | ValueRational !Rational
           | ValueBool !Bool
           | ValueText !Text
           | ValueList !(Seq Value)
           | ValueMap !(Map Text Value)
           deriving (Show, Eq)

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

-- Type displaying

displayValueType :: Value -> Text
displayValueType ValueVoid         = "Void"
displayValueType (ValueInt      _) = "Int"
displayValueType (ValueRational _) = "Rational"
displayValueType (ValueBool     _) = "Bool"
displayValueType (ValueText     _) = "Text"
displayValueType (ValueList     _) = "List"
displayValueType (ValueMap      _) = "Map"

displayValuesType :: [Value] -> Text
displayValuesType lst = "(" <> (Text.intercalate ", " $ displayValueType <$> lst) <> ")"
