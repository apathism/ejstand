module EjStand.ELang
  ( ASTElement(..)
  , buildAST
  , evaluate
  )
where

import           Data.Functor.Identity          ( Identity(..) )
import           Data.Text                      ( Text )
import           EjStand.ELang.AST              ( ASTElement(..) )
import           EjStand.ELang.Eval             ( runEvaluate )
import           EjStand.ELang.Lexer            ( parseLexem )
import           EjStand.ELang.Library          ( defaultBindingMap
                                                , defaultOperatorMeta
                                                )
import           EjStand.ELang.Syntax           ( runSyntaxAnalyzerStrict )
import           EjStand.ELang.Value            ( Value(..) )

buildAST :: Text -> Either Text ASTElement
buildAST source = do
  lexems <- parseLexem source
  runSyntaxAnalyzerStrict defaultOperatorMeta lexems

evaluate :: ASTElement -> Either Text Value
evaluate ast = runIdentity $ runEvaluate defaultBindingMap ast
