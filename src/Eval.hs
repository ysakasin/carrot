module Eval
(
  eval
) where

import Parser
import Object
import AST
import qualified Data.Map as Map
import Text.Trifecta
import Text.PrettyPrint.ANSI.Leijen(putDoc)

type Env = Map.Map String Object

evalAST env (CompoundNode (x:[])) = do
  ll <- evalAST env x
  return ll

evalAST env (CompoundNode (x:xs)) = do
  (envl, lv) <- evalAST env x
  let ll = case lv of Return obj -> return (envl, Return obj)
                      _ -> evalAST envl $ CompoundNode xs
  a <- ll
  return a

evalAST env (IntLitNode x) = do
  return (env, genIntObject x)

evalAST env (BoolLitNode x) = do
  return (env, genBoolObject x)

evalAST env (AssignNode (IdentNode name) r) = do
  (envr, obj) <- evalAST env r
  return (Map.insert name obj envr, obj)

evalAST env (IdentNode name) = do
  let obj = case Map.lookup name env of Just o -> o
                                        Nothing -> genNilObject
  return (env, obj)

evalAST env (DefineFunctionNode name args ast) = do
  let func = genFunctionObject args ast
  return (Map.insert name func env, func)

evalAST env (IfNode conditionAST thenAST elseAST) = do
  (_, obj) <- evalAST env conditionAST
  let BoolValue b = value $ call obj "true?" []
  x <- if b then evalAST env thenAST else evalAST env elseAST
  return x

evalAST env (AddNode l r) = do
  (envl, lv) <- evalAST env  l
  (envr, rv) <- evalAST envl r
  return (envr, Object.call lv "+" [rv])

evalAST env (SubNode l r) = do
  (envl, lv) <- evalAST env  l
  (envr, rv) <- evalAST envl r
  return (envr, Object.call lv "-" [rv])

evalAST env (MulNode l r) = do
  (envl, lv) <- evalAST env  l
  (envr, rv) <- evalAST envl r
  return (envr, Object.call lv "*" [rv])

evalAST env (DivNode l r) = do
  (envl, lv) <- evalAST env  l
  (envr, rv) <- evalAST envl r
  return (envr, Object.call lv "/" [rv])

evalAST env (EqNode l r) = do
  (envl, lv) <- evalAST env  l
  (envr, rv) <- evalAST envl r
  return (envr, Object.call lv "==" [rv])

evalAST env (LessNode l r) = do
  (envl, lv) <- evalAST env  l
  (envr, rv) <- evalAST envl r
  return (envr, Object.call lv ">" [rv])

evalAST env (MoreNode l r) = do
  (envl, lv) <- evalAST env  l
  (envr, rv) <- evalAST envl r
  return (envr, Object.call lv "<" [rv])

evalAST env (AndLessNode l r) = do
  (envl, lv) <- evalAST env  l
  (envr, rv) <- evalAST envl r
  return (envr, Object.call lv ">=" [rv])

evalAST env (AndMoreNode l r) = do
  (envl, lv) <- evalAST env  l
  (envr, rv) <- evalAST envl r
  return (envr, Object.call lv "<=" [rv])

evalAST env (SimpleNode l) = do
  x <- evalAST env l
  return x

evalAST env (CallNode "puts" (param:a)) = do
  (envv, v) <- evalAST env param
  putStrLn $ show v
  return (envv, v)

evalAST env (CallNode name params) = do
  let Just func = Map.lookup name env
  ps <- evalParams env params
  let ASTValue args ast = value func
  let paramList = Map.fromList $ zipWith (\x y -> (x, y)) args ps
  let envLocal = Map.union paramList env
  (_, lv) <- evalAST envLocal ast
  let obj = case lv of Return o -> o
                       x -> x
  return (env, obj)

evalAST env EmptyNode = do
  return (env, genNilObject)

evalAST env (ReturnNode x) = do
  (env, obj) <- evalAST env x
  return (env, Return obj)

evalParams env [] = do
  return []

evalParams env (p:params) = do
  (envr, pp) <- evalAST env p
  ps <- evalParams env params
  return (pp:ps)

eval xs =
  case parse xs of
    Failure doc -> do
      putDoc doc
      putStrLn "Parse error."
    Success ast -> do
      -- print ast
      a <- evalAST Map.empty ast
      return ()
