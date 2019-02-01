module EjStand.ELang
  ( ASTElement(..)
  , Binding(..)
  , ToValue(..)
  , Value(..)
  , buildAST
  , evaluate
  )
where

import           Data.Functor.Identity          ( Identity(..) )
import           Data.Map.Strict               as Map
import           Data.Text                      ( Text )
import           EjStand.ELang.AST              ( ASTElement(..)
                                                , Binding(..)
                                                )
import           EjStand.ELang.Eval             ( runEvaluate )
import           EjStand.ELang.Lexer            ( parseLexem )
import           EjStand.ELang.Library          ( defaultBindingMap
                                                , defaultOperatorMeta
                                                )
import           EjStand.ELang.Syntax           ( runSyntaxAnalyzerStrict )
import           EjStand.ELang.Value            ( ToValue(..)
                                                , Value(..)
                                                )
import           EjStand.Internals.Core         ( fromIdentifiableList )

buildAST :: Text -> Either Text ASTElement
buildAST source = do
  lexems <- parseLexem source
  runSyntaxAnalyzerStrict defaultOperatorMeta lexems

evaluate :: ASTElement -> [Binding Identity] -> Either Text Value
evaluate ast binds =
  let allBindings = fromIdentifiableList binds `Map.union` defaultBindingMap
  in  runIdentity $ runEvaluate allBindings ast
