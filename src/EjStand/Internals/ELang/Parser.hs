{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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
import           Data.Function                  ( on )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           EjStand.Internals.ELang.AST    ( ASTElement(..)
                                                , OperatorMeta(..)
                                                , OperatorAssociativity(..)
                                                )
import           EjStand.Internals.ELang.Lexer  ( Lexem(..) )
import           EjStand.Internals.ELang.Value  ( Value(..) )
import           Safe                           ( tailSafe )

{-
    expression ::= operand | operand operator expression

    operand ::= variable | constant | [elements-list] |
                function (elements-list) | {dictionary-elements} |
                (expression)

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

assertNextLexem :: Lexem -> ParsingState ()
assertNextLexem lexem = do
  peekL <- peekLexem
  if peekL == Just lexem
    then dropLexem
    else throwE . Text.pack $ "Expected " <> show lexem <> ", but found " <> show peekL

getOperator :: Text -> ParsingState OperatorMeta
getOperator name = do
  operators <- getOperators <$> lift get
  case Map.lookup name operators of
    Nothing -> throwE $ "Operator " <> name <> " not found"
    Just op -> return op

-- Parsers

parseExpression :: ParsingState ASTElement
parseExpression = parseExpression' [] []
 where
  applicationOperatorPredicate :: OperatorMeta -> OperatorMeta -> ParsingState Bool
  applicationOperatorPredicate newOp oldOp = case (compare `on` operatorMetaPriority) newOp oldOp of
    GT -> return False
    LT -> return True
    EQ -> if ((/=) `on` operatorMetaAssociativity) newOp oldOp
      then throwE "Operators have different associativity with equal priority within expression segment"
      else case operatorMetaAssociativity newOp of
        LeftAssociativity  -> return True
        RightAssociativity -> return False
        NonAssociative ->
          throwE "Non-associative operators with equal priority can't be mixed up within expression segment"

  applyOperatorMetaWhile
    :: (OperatorMeta -> ParsingState Bool)
    -> [OperatorMeta]
    -> [ASTElement]
    -> ParsingState ([OperatorMeta], [ASTElement])
  applyOperatorMetaWhile _ [] expressionStack = return ([], expressionStack)
  applyOperatorMetaWhile predicateF operatorStack@(x@OperatorMeta {..} : xs) expressionStack = do
    shouldApply <- predicateF x
    if shouldApply
      then case expressionStack of
        (e2 : e1 : eRest) -> applyOperatorMetaWhile predicateF xs ((ASTOperator operatorMetaName (e1, e2)) : eRest)
        _                 -> throwE $ "Not enough arguments supplied for operator " <> operatorMetaName
      else return (operatorStack, expressionStack)

  parseExpression' :: [OperatorMeta] -> [ASTElement] -> ParsingState ASTElement
  parseExpression' operatorStack expressionStack = do
    operand <- parseOperand
    peekLexem >>= \case
      Just (OperatorName name) -> do
        dropLexem
        meta                               <- getOperator name
        (operatorStack', expressionStack') <- applyOperatorMetaWhile (applicationOperatorPredicate meta)
                                                                     operatorStack
                                                                     (operand : expressionStack)
        parseExpression' (meta : operatorStack') expressionStack'
      _ -> applyOperatorMetaWhile (const $ return True) operatorStack (operand : expressionStack) >>= \case
        ([], [exp]) -> return exp
        _           -> throwE "ELang fatal error: Invalid operators/values in stack"

parseOperand :: ParsingState ASTElement
parseOperand = peekLexem >>= \case
  Nothing                    -> throwE "Operand expected, but end of input found"
  Just (VariableName  name ) -> dropLexem >> return (ASTVariable name)
  Just (BoolConst     value) -> dropLexem >> return (ASTConstant (ValueBool value))
  Just (IntegerConst  value) -> dropLexem >> return (ASTConstant (ValueInt value))
  Just (StringLiteral value) -> dropLexem >> return (ASTConstant (ValueText value))
  Just OpenBracket           -> do
    dropLexem
    exp <- parseListArguments
    assertNextLexem CloseBracket
    return (ASTList exp)
  Just (FunctionName value)  -> do
    dropLexem
    assertNextLexem OpenParenthesis
    exp <- parseListArguments
    assertNextLexem CloseParenthesis
    return (ASTFunctionCall value exp)
  Just OpenParenthesis       -> do
    dropLexem
    exp <- parseExpression
    assertNextLexem CloseParenthesis
    return exp
  Just OpenBrace             -> do
    dropLexem
    mapE <- parseMap
    assertNextLexem CloseBrace
    return $ ASTMap mapE
  e                          -> throwE $ "Operand expected, but " <> Text.pack (show e) <> "found"

parseListArguments :: ParsingState [ASTElement]
parseListArguments = do
  exp  <- parseExpression
  peek <- peekLexem
  tail <- case peek of
    Just Comma -> dropLexem >> parseListArguments
    _          -> return []
  return (exp : tail)

parseMap :: ParsingState (Map Text ASTElement)
parseMap = do
  peek1 <- peekLexem
  label <- case peek1 of
    Just (StringLiteral value) -> return value
    _                          -> throwE "String literal as key value expected"
  value <- parseExpression
  peek2 <- peekLexem
  tail  <- case peek2 of
    Just Comma -> dropLexem >> parseMap
    _          -> return Map.empty
  if label `Map.member` tail then throwE "Duplicate key in ELang map value" else return $ Map.insert label value tail
