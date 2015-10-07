module Tokenize
(
  Token(..),
  tokenize
) where

import Data.Char

data Token = IntToken Int
           | IdentToken String
           | ParenthesisBeginToken
           | ParenthesisEndToken
           | KeywordDoToken
           | KeywordEndToken
           | KeywordDefToken
           | DoubleQuotationToken
           | AssignOpToken
           | AddOpToken
           | SubOpToken
           | MulOpToken
           | DivOpToken
           | CommaToken
           | SpaceToken
           | EmptyToken
           | NewLineToken
           | FunctionToken String
           | ArgsToken [String]
           | PrimaryToken [Token]
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = [NewLineToken]
tokenize ('(':xs)  = ParenthesisBeginToken:tokenize(xs)
tokenize (')':xs)  = ParenthesisEndToken:tokenize(xs)
tokenize ('\"':xs) = DoubleQuotationToken:tokenize(xs)
tokenize ('\n':xs) = NewLineToken:tokenize(xs)
tokenize ('=':xs)  = AssignOpToken:tokenize(xs)
tokenize ('+':xs)  = AddOpToken:tokenize(xs)
tokenize ('-':xs)  = SubOpToken:tokenize(xs)
tokenize ('*':xs)  = MulOpToken:tokenize(xs)
tokenize ('/':xs)  = DivOpToken:tokenize(xs)
tokenize (',':xs)  = CommaToken:tokenize(xs)
tokenize (' ':xs)  = tokenize(xs)
tokenize xxs@(x:_)
  | isDigit x = IntToken (read intString::Int):tokenize(beforeDigit)
  | otherwise = IdentToken ident:tokenize(beforeIdent)
  where (intString, beforeDigit) = span isDigit xxs
        (ident, beforeIdent) = span (\a -> isAlphaNum a || notElem a "()\"\n=+-*/, ") xxs
