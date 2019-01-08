{-# LANGUAGE MultiParamTypeClasses #-}
module EjStand.Internals.ELang.AST
  ( ASTElement(..)
  , Binding(..)
  , BindingMap
  , FunctionF
  , OperatorF
  , OperatorMeta(..)
  , OperatorAssociativity(..)
  )
where

import           Control.Monad.Trans.Except     ( ExceptT )
import           Data.Map.Strict                ( Map )
import           Data.Sequence                 as Seq
import           Data.Text                      ( Text )
import           EjStand.Internals.Core         ( IdentifiableBy(..) )
import           EjStand.Internals.ELang.Value  ( Value(..) )

data ASTElement = ASTConstant !Value
                | ASTVariable !Text
                | ASTList !(Seq ASTElement)
                | ASTMap !(Map Text ASTElement)
                | ASTOperator { operatorName      :: !Text
                              , operatorArguments :: !(ASTElement, ASTElement) }
                | ASTFunctionCall { functionName :: !Text
                                  , functionArguments :: ![ASTElement] }

type OperatorF m = Value -> Value -> ExceptT Text m Value
type FunctionF m = [Value] -> ExceptT Text m Value

data Binding m = VariableBinding { bindingName  :: !Text
                                 , bindingValue :: !(m Value)
                                 }
               | OperatorBinding { bindingName     :: !Text
                                 , bindingOperator :: !(OperatorF m)
                                 }
               | FunctionBinding { bindingName     :: !Text
                                 , bindingFunction :: !(FunctionF m)
                                 }

instance IdentifiableBy Text (Binding m) where
  getID = bindingName

type BindingMap m = Map Text (Binding m)

data OperatorMeta = OperatorMeta { operatorMetaName          :: !Text
                                 , operatorMetaPriority      :: !Int
                                 , operatorMetaAssociativity :: !OperatorAssociativity
                                 }

instance IdentifiableBy Text OperatorMeta where
  getID = operatorMetaName

data OperatorAssociativity = LeftAssociativity | RightAssociativity | NonAssociative
  deriving (Eq)
