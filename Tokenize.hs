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
           | KeywordIfToken
           | KeywordThenToken
           | ReturnToken
           | DoubleQuotationToken
           | AssignOpToken
           | AddOpToken
           | SubOpToken
           | MulOpToken
           | DivOpToken
           | EqOpToken
           | CommaToken
           | SpaceToken
           | TrueToken
           | FalseToken
           | EmptyToken
           | NewLineToken
           | ArgsToken [String]
           | ParenthesisToken [Token]
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = [NewLineToken]
tokenize ('=':'=':xs) = EqOpToken:tokenize(xs)
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
tokenize ('d':'e':'f':xs)     = KeywordDefToken:tokenize(xs)
tokenize ('i':'f':xs)         = KeywordIfToken:tokenize(xs)
tokenize ('t':'h':'e':'n':xs) = KeywordThenToken:tokenize(xs)
tokenize ('d':'o':xs)         = KeywordDoToken:tokenize(xs)
tokenize ('e':'n':'d':xs)     = KeywordEndToken:tokenize(xs)
tokenize ('t':'r':'u':'e':xs)     = TrueToken:tokenize(xs)
tokenize ('f':'a':'l':'s':'e':xs) = FalseToken:tokenize(xs)
tokenize ('r':'e':'t':'u':'r':'n':xs) = ReturnToken:tokenize(xs)
tokenize (' ':xs)  = tokenize(xs)
tokenize xxs@(x:_)
  | isDigit x = IntToken (read intString::Int):tokenize(beforeDigit)
  | otherwise = IdentToken ident:tokenize(beforeIdent)
  where (intString, beforeDigit) = span isDigit xxs
        (ident, beforeIdent) = span (\a -> isAlphaNum a || notElem a "()\"\n=+-*/, ") xxs
