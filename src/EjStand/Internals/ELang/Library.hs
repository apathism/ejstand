{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module EjStand.Internals.ELang.Library
  ( defaultBindingMap
  , defaultOperatorMeta
  )
where

import           Control.Monad.Trans.Except     ( ExceptT(..)
                                                , throwE
                                                )
import           Data.Fixed                     ( mod' )
import           Data.Functor.Identity          ( Identity )
import           Data.Map.Strict               as Map
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           Data.Text                      ( Text )
import           Data.Text                     as Text
import           Data.Sequence                 as Seq
import           EjStand.Internals.Core         ( fromIdentifiableList )
import           EjStand.Internals.ELang.AST    ( Binding(..)
                                                , BindingMap
                                                , OperatorAssociativity(..)
                                                , OperatorMeta(..)
                                                )
import           EjStand.Internals.ELang.Value  ( FromValue(..)
                                                , ToValue(..)
                                                , Value(..)
                                                , displayValuesType
                                                )
import           Safe                           ( headMay )

-- Helper functions

class NativeFunction f where
  fromNative :: f -> [Value] -> Maybe Value

instance ToValue a => NativeFunction a where
  fromNative result [] = Just . toValue $ result
  fromNative _      _  = Nothing

instance {-# OVERLAPPING #-} (FromValue a, NativeFunction r) => NativeFunction (a -> r) where
  fromNative _    []                        = Nothing
  fromNative func (firstValue : tailValues) = do
    val <- fromValue firstValue
    fromNative (func val) tailValues

invalidArgumentsError :: Monad m => Text -> [Value] -> ExceptT Text m a
invalidArgumentsError name args = throwE $ mconcat [name, " doesn't take arguments of type ", displayValuesType args]

mergeNativeToFunction :: Monad m => [[Value] -> Maybe Value] -> Text -> [Value] -> ExceptT Text m Value
mergeNativeToFunction handlers name args = case headMay $ catMaybes [ handler args | handler <- handlers ] of
  Nothing -> invalidArgumentsError name args
  Just x  -> return x

mergeNativeToOperator :: Monad m => [[Value] -> Maybe Value] -> OperatorMeta -> Value -> Value -> ExceptT Text m Value
mergeNativeToOperator handlers OperatorMeta {..} v1 v2 = mergeNativeToFunction handlers operatorMetaName [v1, v2]

-- Binding builders

type CombinedOperator m = (OperatorMeta, Value -> Value -> ExceptT Text m Value)
type CombinedFunction m = (Text, [Value] -> ExceptT Text m Value)

(==>) :: a -> (a -> b) -> (a, b)
value ==> gen = (value, gen value)

operatorToBinding :: Monad m => CombinedOperator m -> Binding m
operatorToBinding (OperatorMeta {..}, func) = OperatorBinding operatorMetaName func

functionToBinding :: Monad m => CombinedFunction m -> Binding m
functionToBinding (name, func) = FunctionBinding name func

{-

  Standard library operators

  +---+------+------------------------------------------------+----------+
  | P | Sign | Description                                    | Operands |
  +---+------+------------------------------------------------+----------+
  | 9 |  ->  | Indexing                                       |  I  TLM  |
  | 8 |  **  | Power                                          |  IR      |
  | 7 |  *   | Multiplication                                 |  IR      |
  |   |  /   | Division                                       |  IR      |
  |   |  %   | Modulo                                         |  IR      |
  |   |  //  | Integer division                               |  IR      |
  | 6 |  +   | Plus                                           |  IR      |
  |   |  -   | Minus                                          |  IR      |
  | 5 |  ++  | Concatentation                                 |     TL   |
  | 4 |  <   | Less                                           |  IR T    |
  |   |  <=  | Less or equal                                  |  IR T    |
  |   |  >   | Greater                                        |  IR T    |
  |   |  >=  | Greater of equal                               |  IR T    |
  |   |  ==  | Equal                                          | VIRBTLM  |
  |   |  !=  | Not equal                                      | VIRBTLM  |
  | 3 |  &&  | Logical and                                    |    B     |
  | 2 |  ||  | Logical or                                     |    B     |
  | 1 |  ;   | Sequence of computations ignoring left result  | VIRBTLM  |
  +---+------+------------------------------------------------+----------+

-}

operatorIndex :: Monad m => CombinedOperator m
operatorIndex = OperatorMeta "->" 9 LeftAssociativity ==> operatorF
 where
  operatorF :: Monad m => OperatorMeta -> Value -> Value -> ExceptT Text m Value
  operatorF _ (ValueText text) (ValueInt index) = return $ if index < 0 || index >= toInteger (Text.length text)
    then ValueVoid
    else ValueText . Text.singleton . Text.index text . fromInteger $ index
  operatorF _ (ValueList lst) (ValueInt index) = return . fromMaybe ValueVoid $ fromInteger index `Seq.lookup` lst
  operatorF _ (ValueMap map) (ValueText key) = return . fromMaybe ValueVoid $ key `Map.lookup` map
  operatorF OperatorMeta {..} v1 v2 = invalidArgumentsError operatorMetaName [v1, v2]

operatorPower :: Monad m => CombinedOperator m
operatorPower = OperatorMeta "**" 8 RightAssociativity
  ==> mergeNativeToOperator [fromNative ((^^) :: Rational -> Integer -> Rational)]

operatorMultiplication :: Monad m => CombinedOperator m
operatorMultiplication = OperatorMeta "*" 7 LeftAssociativity ==> mergeNativeToOperator
  [fromNative ((*) :: Integer -> Integer -> Integer), fromNative ((*) :: Rational -> Rational -> Rational)]

operatorDivision :: Monad m => CombinedOperator m
operatorDivision =
  OperatorMeta "/" 7 LeftAssociativity ==> mergeNativeToOperator [fromNative ((/) :: Rational -> Rational -> Rational)]

operatorModulo :: Monad m => CombinedOperator m
operatorModulo = OperatorMeta "%" 7 LeftAssociativity ==> mergeNativeToOperator
  [fromNative (mod :: Integer -> Integer -> Integer), fromNative (mod' :: Rational -> Rational -> Rational)]

operatorIntegerDivision :: Monad m => CombinedOperator m
operatorIntegerDivision =
  OperatorMeta "//" 7 LeftAssociativity ==> mergeNativeToOperator [fromNative (div :: Integer -> Integer -> Integer)]

operatorPlus :: Monad m => CombinedOperator m
operatorPlus = OperatorMeta "+" 6 LeftAssociativity ==> mergeNativeToOperator
  [fromNative ((+) :: Integer -> Integer -> Integer), fromNative ((+) :: Rational -> Rational -> Rational)]

operatorMinus :: Monad m => CombinedOperator m
operatorMinus = OperatorMeta "-" 6 LeftAssociativity ==> mergeNativeToOperator
  [fromNative ((-) :: Integer -> Integer -> Integer), fromNative ((-) :: Rational -> Rational -> Rational)]

operatorConcat :: Monad m => CombinedOperator m
operatorConcat = OperatorMeta "++" 5 LeftAssociativity ==> operatorF
 where
  operatorF :: Monad m => OperatorMeta -> Value -> Value -> ExceptT Text m Value
  operatorF _                 (ValueText s1) (ValueText s2) = return . ValueText $ s1 <> s2
  operatorF _                 (ValueList l1) (ValueList l2) = return . ValueList $ l1 <> l2
  operatorF OperatorMeta {..} v1             v2             = invalidArgumentsError operatorMetaName [v1, v2]

operatorLess :: Monad m => CombinedOperator m
operatorLess = OperatorMeta "<" 4 NonAssociative ==> mergeNativeToOperator
  [ fromNative ((<) :: Integer -> Integer -> Bool)
  , fromNative ((<) :: Rational -> Rational -> Bool)
  , fromNative ((<) :: Text -> Text -> Bool)
  ]

operatorLessOrEqual :: Monad m => CombinedOperator m
operatorLessOrEqual = OperatorMeta "<=" 4 NonAssociative ==> mergeNativeToOperator
  [ fromNative ((<=) :: Integer -> Integer -> Bool)
  , fromNative ((<=) :: Rational -> Rational -> Bool)
  , fromNative ((<=) :: Text -> Text -> Bool)
  ]


operatorGreater :: Monad m => CombinedOperator m
operatorGreater = OperatorMeta ">" 4 NonAssociative ==> mergeNativeToOperator
  [ fromNative ((>) :: Integer -> Integer -> Bool)
  , fromNative ((>) :: Rational -> Rational -> Bool)
  , fromNative ((>) :: Text -> Text -> Bool)
  ]

operatorGreaterOrEqual :: Monad m => CombinedOperator m
operatorGreaterOrEqual = OperatorMeta ">=" 4 NonAssociative ==> mergeNativeToOperator
  [ fromNative ((>=) :: Integer -> Integer -> Bool)
  , fromNative ((>=) :: Rational -> Rational -> Bool)
  , fromNative ((>=) :: Text -> Text -> Bool)
  ]

operatorEqual :: Monad m => CombinedOperator m
operatorEqual = (OperatorMeta "==" 4 NonAssociative, \v1 v2 -> return . ValueBool $ v1 == v2)

operatorNotEqual :: Monad m => CombinedOperator m
operatorNotEqual = (OperatorMeta "!=" 4 NonAssociative, \v1 v2 -> return . ValueBool $ v1 /= v2)

operatorAnd :: Monad m => CombinedOperator m
operatorAnd = OperatorMeta "&&" 3 LeftAssociativity ==> mergeNativeToOperator [fromNative (&&)]

operatorOr :: Monad m => CombinedOperator m
operatorOr = OperatorMeta "||" 2 LeftAssociativity ==> mergeNativeToOperator [fromNative (||)]

operatorSeq :: Monad m => CombinedOperator m
operatorSeq = (OperatorMeta ";" 1 LeftAssociativity, operatorF)
 where
  operatorF :: Monad m => Value -> Value -> ExceptT Text m Value
  operatorF !_ = return

-- Operator & Function compilations

allCombinedOperators :: Monad m => [CombinedOperator m]
allCombinedOperators =
  [ operatorIndex
  , operatorPower
  , operatorMultiplication
  , operatorDivision
  , operatorModulo
  , operatorIntegerDivision
  , operatorPlus
  , operatorMinus
  , operatorConcat
  , operatorLess
  , operatorLessOrEqual
  , operatorGreater
  , operatorGreaterOrEqual
  , operatorEqual
  , operatorNotEqual
  , operatorAnd
  , operatorOr
  , operatorSeq
  ]

allFunctionBindings :: Monad m => [Binding m]
allFunctionBindings = []

defaultOperatorMeta :: [OperatorMeta]
defaultOperatorMeta = fst <$> (allCombinedOperators :: [CombinedOperator Identity])

defaultBindingMap :: Monad m => BindingMap m
defaultBindingMap = fromIdentifiableList $ allFunctionBindings <> (operatorToBinding <$> allCombinedOperators)
