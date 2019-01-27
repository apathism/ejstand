module EjStand.Internals.ELang
  ( ASTElement(..)
  , buildAST
  , evaluate
  )
where

import           Data.Functor.Identity          ( Identity(..) )
import           Data.Text                      ( Text )
import           EjStand.Internals.ELang.AST    ( ASTElement(..) )
import           EjStand.Internals.ELang.Eval   ( runEvaluate )
import           EjStand.Internals.ELang.Lexer  ( parseLexem )
import           EjStand.Internals.ELang.Library
                                                ( defaultBindingMap
                                                , defaultOperatorMeta
                                                )
import           EjStand.Internals.ELang.Syntax ( runSyntaxAnalyzerStrict )
import           EjStand.Internals.ELang.Value  ( Value(..) )

buildAST :: Text -> Either Text ASTElement
buildAST source = do
  lexems <- parseLexem source
  runSyntaxAnalyzerStrict defaultOperatorMeta lexems

evaluate :: ASTElement -> Either Text Value
evaluate ast = runIdentity $ runEvaluate defaultBindingMap ast
