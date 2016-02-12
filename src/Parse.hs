module Parse
(
  parse
) where

import Tokenize
import Object
import AST

import qualified Data.Map as Map


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
parseStatements tss@(KeywordElseToken:_) = (CompoundNode [], tss)
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
parseStatement [] (KeywordIfToken:ts) = (IfNode conditionAST stmtsAST elseStmtsAST, afterElse)
  where (conditionExpr, KeywordThenToken:afterTs) = break (\x -> KeywordThenToken == x || NewLineToken == x) ts
        conditionAST = parseExpression conditionExpr
        (stmtsAST, afterIf) = parseStatements afterTs
        afterhead:aaa = afterIf
        _:bbb = aaa
        (elseStmtsAST, afterElse) = if afterhead == KeywordElseToken then parseStatements bbb else (EmptyNode, afterIf)
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
parseExpression ts@(SubOpToken:_) = parseTerm ts
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
                                 DivOpToken -> DivNode (parseTerm avl) $ parsePrimaryExpression avr
  | otherwise = parsePrimaryExpression ts
  where (ravr, ravl) = break (\x -> MulOpToken == x || DivOpToken == x) $ reverse ts
        x:nravl = ravl
        avl = reverse nravl
        avr = reverse ravr

parsePrimaryExpression :: [Token] -> AST
parsePrimaryExpression (SubOpToken:IntToken x:[]) = IntLitNode $ negate x
parsePrimaryExpression [IntToken x] = IntLitNode x
parsePrimaryExpression (IdentToken x:ParenthesisToken y:[]) = CallNode x paramsAST
  where paramsAST = getParams y
parsePrimaryExpression [IdentToken x] = IdentNode x
parsePrimaryExpression [ParenthesisToken x] = parseExpression x
parsePrimaryExpression [TrueToken] = BoolLitNode True
parsePrimaryExpression [FalseToken] = BoolLitNode False
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
