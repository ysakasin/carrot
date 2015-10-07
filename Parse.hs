module Parse
(
  AST(..),
  parse
) where

import Tokenize

import qualified Data.Map as Map

data PrimaryExpression = PrimaryInt Int
                       | Ident String
                       | PrimaryParenthesis Expression
  deriving (Show)

data Term = SimpleTerm PrimaryExpression
          | MulTerm PrimaryExpression Term
          | DivTerm PrimaryExpression Term
  deriving (Show)

data Expression = AddExpression Term Expression
                | SubExpression Term Expression
                | SimpleExpression Term
  deriving (Show)

data AST = SimpleNode AST
         | AssignNode AST AST
         | AddNode AST AST
         | SubNode AST AST
         | MulNode AST AST
         | DivNode AST AST
         | IntValueNode Int
         | IdentNode String
         | CompoundNode [AST]
  deriving (Show)

tokenizePrimaryToken :: [Token] -> [Token]
tokenizePrimaryToken (ParenthesisBeginToken:ts) = (PrimaryToken inner):(tokenizePrimaryToken outer)
  where (inner, outer) = searchPEndToken(ts)
tokenizePrimaryToken [] = []
tokenizePrimaryToken (t:ts) = t:(tokenizePrimaryToken ts)

searchPEndToken :: [Token] -> ([Token], [Token])
searchPEndToken [] = error "Can not fined end token"
searchPEndToken (ParenthesisEndToken:ts) = ([], ts)
searchPEndToken (ParenthesisBeginToken:ts) = searchPEndToken tokens
  where tokens = tokenizePrimaryToken $ ParenthesisBeginToken:ts
searchPEndToken (t:ts) = (t:token, beforeToken)
  where (token, beforeToken) = searchPEndToken(ts)

parseStatements :: [Token] -> AST
parseStatements [] = CompoundNode []
parseStatements ts = CompoundNode (ast:asts)
  where (stmt, NewLineToken:stmts) = break (NewLineToken ==) ts
        ast = parseStatement stmt
        CompoundNode asts = parseStatements stmts

parseStatement :: [Token] -> AST
parseStatement ts = parseExpression expr
    where expr = ts

parseExpression :: [Token] -> AST
parseExpression ((IdentToken x):AssignOpToken:ts) = AssignNode (IdentNode x) $ parseExpression ts
parseExpression vs
  | not $ ravl == [] = case x of AddOpToken -> AddNode (parseExpression avl) $ parseTerm avr
                                 SubOpToken -> SubNode (parseExpression avl) $ parseTerm avr
  | otherwise = parseTerm vs
  where (ravr, ravl) = break (\x -> AddOpToken == x || SubOpToken == x) $ reverse vs
        x:nravl = ravl
        avl = reverse nravl
        avr = reverse ravr

parseTerm :: [Token] -> AST
parseTerm ts
  | not $ ravl == [] = case x of MulOpToken -> MulNode (parseTerm avl) $ parsePrimaryExpression avr
                                 SubOpToken -> DivNode (parseTerm avl) $ parsePrimaryExpression avr
  | otherwise = parsePrimaryExpression ts
  where (ravr, ravl) = break (\x -> MulOpToken == x || DivOpToken == x) $ reverse ts
        x:nravl = ravl
        avl = reverse nravl
        avr = reverse ravr

parsePrimaryExpression :: [Token] -> AST
parsePrimaryExpression [IntToken x] = IntValueNode x
parsePrimaryExpression [IdentToken x] = IdentNode x
parsePrimaryExpression [PrimaryToken x] = parseExpression x
parsePrimaryExpression _ = error "Parse Error"

parse :: String -> AST
parse xs = parseStatements $ tokenizePrimaryToken $ tokenize xs
