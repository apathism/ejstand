{-# LANGUAGE TypeFamilies #-}
module EjStand.ELang.AST
  ( ASTElement(..)
  , Binding(..)
  , BindingMap
  , OperatorMeta(..)
  , OperatorAssociativity(..)
  )
where

import           Control.Monad.Trans.Except     ( ExceptT )
import           Data.Map.Strict                ( Map )
import           Data.Sequence                 as Seq
import           Data.Text                      ( Text )
import           EjStand.ELang.Value            ( Value(..) )
import           EjStand.Internals.Core         ( Identifiable(..) )

data ASTElement = ASTConstant !Value
                | ASTVariable !Text
                | ASTList !(Seq ASTElement)
                | ASTMap !(Map Text ASTElement)
                | ASTOperator { operatorName      :: !Text
                              , operatorArguments :: !(ASTElement, ASTElement) }
                | ASTFunctionCall { functionName :: !Text
                                  , functionArguments :: ![ASTElement] }
                deriving (Show)

data Binding m = VariableBinding { bindingName  :: !Text
                                 , bindingValue :: m Value
                                 }
               | OperatorBinding { bindingName     :: !Text
                                 , bindingOperator :: Value -> Value -> ExceptT Text m Value
                                 }
               | FunctionBinding { bindingName     :: !Text
                                 , bindingFunction :: [Value] -> ExceptT Text m Value
                                 }

instance Identifiable (Binding m) where
  type Identificator (Binding m) = Text
  getID = bindingName

type BindingMap m = Map Text (Binding m)

data OperatorMeta = OperatorMeta { operatorMetaName          :: !Text
                                 , operatorMetaPriority      :: !Int
                                 , operatorMetaAssociativity :: !OperatorAssociativity
                                 }

instance Identifiable OperatorMeta where
  type Identificator OperatorMeta = Text
  getID = operatorMetaName

data OperatorAssociativity = LeftAssociativity | RightAssociativity | NonAssociative
  deriving (Eq)
