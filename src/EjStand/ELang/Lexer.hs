{-# Language OverloadedStrings #-}
module EjStand.ELang.Lexer
  ( Lexem(..)
  , parseLexem
  )
where

import           Data.Char                      ( isDigit
                                                , isLetter
                                                , isLower
                                                , isPunctuation
                                                , isSpace
                                                , isSymbol
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Read                 ( decimal )
import           EjStand.Internals.Core         ( (|||) )

data Lexem = OpenParenthesis
           | CloseParenthesis
           | OpenBracket
           | CloseBracket
           | OpenBrace
           | CloseBrace
           | Comma
           | Colon
           | IntegerConst !Integer
           | BoolConst !Bool
           | StringLiteral !Text
           | FunctionName !Text
           | VariableName !Text
           | OperatorName !Text
           deriving (Show, Eq)

readEscapeSequence :: Char -> Either Text Text
readEscapeSequence 'n'  = Right "\n"
readEscapeSequence 't'  = Right "\t"
readEscapeSequence '\'' = Right "\'"
readEscapeSequence '\"' = Right "\""
readEscapeSequence '\\' = Right "\\"
readEscapeSequence _    = Left "Unknown escape sequence in ELang string literal"

readStringLiteral :: Char -> Text -> Either Text (Lexem, Text)
readStringLiteral c source = (\(l, r) -> (StringLiteral l, r)) <$> readStringLiteral' c source
 where
  readStringLiteral' :: Char -> Text -> Either Text (Text, Text)
  readStringLiteral' c source =
    let (before, after) = Text.span (\x -> x /= c && x /= '\\') source
    in  case Text.uncons after of
          Nothing           -> Left "Missing closing quotation mark for string literal"
          Just (c', after') -> if c == c'
            then Right (before, after')
            else case Text.uncons after' of
              Nothing             -> Left "Unexpected end of input after escape character"
              Just (c'', after'') -> do
                escape <- readEscapeSequence c''
                rest   <- readStringLiteral' c after''
                Right (before <> escape <> fst rest, snd rest)

parseLexem :: Text -> Either Text [Lexem]
parseLexem source = case Text.uncons source of
  Nothing        -> Right []
  Just (c, rest) -> case c of
    '(' -> (OpenParenthesis :) <$> parseLexem rest
    ')' -> (CloseParenthesis :) <$> parseLexem rest
    '[' -> (OpenBracket :) <$> parseLexem rest
    ']' -> (CloseBracket :) <$> parseLexem rest
    '{' -> (OpenBrace :) <$> parseLexem rest
    '}' -> (CloseBrace :) <$> parseLexem rest
    ',' -> (Comma :) <$> parseLexem rest
    ':' -> (Colon :) <$> parseLexem rest
    _
      | isSpace c -> parseLexem $ Text.dropWhile isSpace source
      | c `elem` ['\'', '"'] -> do
        (l, r) <- readStringLiteral c rest
        t      <- parseLexem r
        Right (l : t)
      | isDigit c -> do
        (number, r) <- either (Left . Text.pack) Right $ decimal source
        t           <- parseLexem r
        Right (IntegerConst number : t)
      | isPunctuation c || isSymbol c -> do
        let (l, r) = Text.span (isPunctuation ||| isSymbol) source
        t <- parseLexem r
        Right (OperatorName l : t)
      | isLetter c -> do
        let (l, r) = Text.span isLetter source
        right <- parseLexem r
        case l of
          "False" -> Right (BoolConst False : right)
          _       -> Right $ (if isLower c then VariableName else FunctionName) l : right
      | otherwise -> Left $ "Unknown character '" <> Text.singleton c <> "' in ELang input"
