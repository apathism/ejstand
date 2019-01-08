{-# LANGUAGE OverloadedStrings #-}
module EjStand.Internals.ELang.Eval
  ( runEvaluate
  )
where

import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Except     ( ExceptT
                                                , runExceptT
                                                , throwE
                                                )
import           Data.Map.Strict                ( (!?) )
import           Data.Text                      ( Text )
import           EjStand.Internals.ELang.AST    ( ASTElement(..)
                                                , Binding(..)
                                                , BindingMap
                                                )
import           EjStand.Internals.ELang.Value  ( Value(..) )


evaluate :: Monad m => BindingMap m -> ASTElement -> ExceptT Text m Value
evaluate bindings ast = case ast of
  (ASTConstant value                ) -> return value
  (ASTVariable name                 ) -> case bindings !? name of
    Just (VariableBinding _ value) -> lift value
    _                              -> throwE $ "Undefined variable " <> name
  (ASTList     elements             ) -> ValueList <$> sequence (evaluate bindings <$> elements)
  (ASTMap      elements             ) -> ValueMap <$> sequence (evaluate bindings <$> elements)
  (ASTOperator     name (argL, argR)) -> case bindings !? name of
    Just (OperatorBinding _ operatorF) -> do
      valueL <- evaluate bindings argL
      valueR <- evaluate bindings argR
      operatorF valueL valueR
    _                                  -> throwE $ "Undefined operator" <> name
  (ASTFunctionCall name arguments   ) -> case bindings !? name of
    Just (FunctionBinding _ function) -> sequence (evaluate bindings <$> arguments) >>= function
    _                                 -> throwE $ "Undefined function" <> name

runEvaluate :: Monad m => BindingMap m -> ASTElement -> m (Either Text Value)
runEvaluate bindings = runExceptT . evaluate bindings
