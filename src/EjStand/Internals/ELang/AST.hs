{-# LANGUAGE MultiParamTypeClasses #-}
module EjStand.Internals.ELang.AST
  ( ASTElement(..)
  , Binding(..)
  , OperatorMeta(..)
  )
where

import           Control.Monad.Trans.Except     ( ExceptT )
import           Data.Map.Strict                ( Map )
import           Data.Sequence                  ( Seq )
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
                                  , functionArguments :: !(Seq ASTElement) }

data Binding m = VariableBinding { bindingName  :: !Text
                                 , bindingValue :: !(m Value)
                                 }
               | OperatorBinding { bindingName     :: !Text
                                 , bindingOperator :: !(Value -> Value -> ExceptT Text m Value)
                                 }
               | FunctionBinding { bindingName     :: !Text
                                 , bindingFunction :: !(Seq Value -> ExceptT Text m Value)
                                 }

instance IdentifiableBy Text (Binding m) where
  getID = bindingName

data OperatorMeta = OperatorMeta { operatorMetaName     :: !Text
                                 , operatorMetaPriority :: !Int
                                 }

instance IdentifiableBy Text OperatorMeta where
  getID = operatorMetaName
