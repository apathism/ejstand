{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module EjStand.Internals.ELang.Parser where

import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Except     ( ExceptT
                                                , throwE
                                                )
import           Control.Monad.Trans.State.Strict
                                                ( State
                                                , get
                                                , modify'
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Text                      ( Text )
import           EjStand.Internals.ELang.AST    ( ASTElement(..)
                                                , Binding(..)
                                                , OperatorMeta(..)
                                                )
import           EjStand.Internals.ELang.Lexer  ( Lexem(..) )
import           EjStand.Internals.ELang.Value  ( Value(..) )
import           Safe                           ( tailSafe )

{-
    expression ::= operand | operand operator expression

    operand ::= variable | constant | [elements-list] |
                function (elements-list) | {dictionary-elements}

    elements-list ::= expression | expression , elements-list

    dictionary-elements := dictionary-element |
                           dictionary-element , dictionary-elements
    
    dictionary-element := key-string : expression
-}

data ASTParsingState = ASTParsingState { getLexems    :: [Lexem]
                                       , getOperators :: Map Text OperatorMeta }

type ParsingState = ExceptT Text (State ASTParsingState)

-- Helper functions

peekLexem :: ParsingState (Maybe Lexem)
peekLexem = do
  lexems <- getLexems <$> lift get
  case lexems of
    []      -> return Nothing
    (h : _) -> return $ Just h

dropLexem :: ParsingState ()
dropLexem = lift $ modify' (\state -> state { getLexems = tailSafe $ getLexems state })

getOperator :: Text -> ParsingState OperatorMeta
getOperator name = do
  operators <- getOperators <$> lift get
  case Map.lookup name operators of
    Nothing -> throwE $ "Operator " <> name <> " not found"
    Just op -> return op

-- Parsers

parseOperand :: ParsingState ASTElement
parseOperand = peekLexem >>= \case
  Nothing                   -> throwE "Operand expected, but end of input found"
  Just (VariableName name ) -> dropLexem >> return (ASTVariable name)
  Just (BoolConst    value) -> dropLexem >> return (ASTConstant (ValueBool value))
