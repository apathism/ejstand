{-# LANGUAGE MultiParamTypeClasses #-}
module EjStand.Internals.ELang.AST
  ( ASTElement(..)
  , Binding(..)
  , OperatorMeta(..)
  , OperatorAssociativity(..)
  )
where

import           Control.Monad.Trans.Except     ( ExceptT )
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import           EjStand.Internals.Core         ( IdentifiableBy(..) )
import           EjStand.Internals.ELang.Value  ( Value(..) )

data ASTElement = ASTConstant !Value
                | ASTVariable !Text
                | ASTList ![ASTElement]
                | ASTMap !(Map Text ASTElement)
                | ASTOperator { operatorName      :: !Text
                              , operatorArguments :: !(ASTElement, ASTElement) }
                | ASTFunctionCall { functionName :: !Text
                                  , functionArguments :: ![ASTElement] }

data Binding m = VariableBinding { bindingName  :: !Text
                                 , bindingValue :: !(m Value)
                                 }
               | OperatorBinding { bindingName     :: !Text
                                 , bindingOperator :: !(Value -> Value -> ExceptT Text m Value)
                                 }
               | FunctionBinding { bindingName     :: !Text
                                 , bindingFunction :: !([Value] -> ExceptT Text m Value)
                                 }

instance IdentifiableBy Text (Binding m) where
  getID = bindingName

data OperatorMeta = OperatorMeta { operatorMetaName          :: !Text
                                 , operatorMetaPriority      :: !Int
                                 , operatorMetaAssociativity :: !OperatorAssociativity
                                 }

instance IdentifiableBy Text OperatorMeta where
  getID = operatorMetaName

data OperatorAssociativity = LeftAssociativity | RightAssociativity | NonAssociative
  deriving (Eq)
