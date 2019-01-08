{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
module EjStand.Internals.ELang.Library where

import           Control.Monad.Trans.Except     ( throwE )
import           EjStand.Internals.ELang.AST    ( Binding(..)
                                                , OperatorAssociativity(..)
                                                , OperatorF
                                                , OperatorMeta(..)
                                                )
import           EjStand.Internals.ELang.Value  ( Value(..) )

-- Helper functions

type CombinedOperator m = (OperatorMeta, OperatorF m)

mkOperatorBinding :: Monad m => CombinedOperator m -> Binding m
mkOperatorBinding (meta, operatorF) =
  OperatorBinding { bindingName = operatorMetaName meta, bindingOperator = operatorF }

mkOperatorNum :: Monad m => OperatorMeta -> (forall a . Num a => a -> a -> a) -> CombinedOperator m
mkOperatorNum meta hf = (meta, mkOperatorNum' meta hf)
 where
  mkOperatorNum' :: Monad m => OperatorMeta -> (forall a . Num a => a -> a -> a) -> OperatorF m
  mkOperatorNum' _ hf (ValueRational v1) (ValueRational v2) = return . ValueRational $ hf v1 v2
  mkOperatorNum' _ hf (ValueInt v1) (ValueRational v2) = return . ValueRational $ hf (fromInteger v1) v2
  mkOperatorNum' _ hf (ValueRational v1) (ValueInt v2) = return . ValueRational $ hf v1 (fromInteger v2)
  mkOperatorNum' meta _ _ _ = throwE $ "Invalid type of arguments for operator " <> operatorMetaName meta

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

operatorPlus :: Monad m => CombinedOperator m
operatorPlus = mkOperatorNum (OperatorMeta "+" 6 LeftAssociativity) (+)

operatorMinus :: Monad m => CombinedOperator m
operatorMinus = mkOperatorNum (OperatorMeta "-" 6 LeftAssociativity) (-)

operatorMultiplication :: Monad m => CombinedOperator m
operatorMultiplication = mkOperatorNum (OperatorMeta "*" 7 LeftAssociativity) (*)
