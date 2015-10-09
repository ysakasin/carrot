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
         | EqNode AST AST
         | IntValueNode Int
         | BoolValueNode Bool
         | IdentNode String
         | CompoundNode [AST]
         | DefineFunctionNode String [String] AST
         | FunctionNode String [String] AST
         | IfNode AST AST AST
         | EmptyNode
         | CallNode String [AST]
         | ReturnNode AST
  deriving (Show)

tokenizePrimaryToken :: [Token] -> [Token]
tokenizePrimaryToken (ParenthesisBeginToken:ts) = (ParenthesisToken inner):(tokenizePrimaryToken outer)
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

parseStatements :: [Token] -> (AST, [Token])
parseStatements [] = (CompoundNode [], [])
parseStatements (KeywordEndToken:NewLineToken:ts) = (CompoundNode [], ts)
parseStatements ts = (CompoundNode $ ast:asts, afterTs)
  where (ast, after) = parseStatement [] ts
        (CompoundNode asts, afterTs) = parseStatements after

parseStatement :: [Token] -> [Token] -> (AST, [Token])
parseStatement token (NewLineToken:ts) = (exprAST, ts)
  where exprAST = parseExpression $ reverse token
parseStatement token (KeywordThenToken:ts) = (exprAST, ts)
  where exprAST = parseExpression $ reverse token
parseStatement [] (KeywordDefToken:ts) = (DefineFunctionNode name args stmtsAST, afterTs)
  where IdentToken name:ParenthesisToken argTokens:NewLineToken:stmt = ts
        args = getArgs argTokens
        (stmtsAST, afterTs) = parseStatements stmt
parseStatement [] (KeywordIfToken:ts) = (IfNode conditionAST stmtsAST EmptyNode, afterIf)
  where (conditionExpr, KeywordThenToken:afterTs) = break (\x -> KeywordThenToken == x || NewLineToken == x) ts
        conditionAST = parseExpression conditionExpr
        (stmtsAST, afterIf) = parseStatements afterTs
parseStatement [] (ReturnToken:ts) = (ReturnNode ast, after)
  where ast = parseExpression r
        (r, after) = break (NewLineToken ==) ts
parseStatement token (t:ts) = parseStatement (t:token) ts

getArgs :: [Token] -> [String]
getArgs [] = []
getArgs [IdentToken x] = [x]
getArgs ((IdentToken x):CommaToken:ts) = x:(getArgs ts)

parseExpression :: [Token] -> AST
parseExpression ((IdentToken x):AssignOpToken:ts) = AssignNode (IdentNode x) $ parseExpression ts
parseExpression vs
  | not $ ravl == [] = case x of AddOpToken -> AddNode (parseExpression avl) $ parseTerm avr
                                 SubOpToken -> SubNode (parseExpression avl) $ parseTerm avr
                                 EqOpToken  -> EqNode  (parseExpression avl) $ parseTerm avr
  | otherwise = parseTerm vs
  where (ravr, ravl) = break (\x -> AddOpToken == x || SubOpToken == x || EqOpToken == x) $ reverse vs
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
parsePrimaryExpression (IdentToken x:ParenthesisToken y:[]) = CallNode x paramsAST
  where paramsAST = getParams y
parsePrimaryExpression [IdentToken x] = IdentNode x
parsePrimaryExpression [ParenthesisToken x] = parseExpression x
parsePrimaryExpression [] = EmptyNode
parsePrimaryExpression _ = error "Parse Error"

getParams :: [Token] -> [AST]
getParams [] = []
getParams ts = if xs == [] then [parseExpression x] else (parseExpression x):(getParams xs)
  where (x, xs) = break (CommaToken ==) ts
        CommaToken:xss = xs

parse :: String -> AST
parse xs = ast
  where (ast, []) = parseStatements $ tokenizePrimaryToken $ tokenize xs
