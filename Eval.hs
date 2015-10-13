module Eval
(
  eval
) where

import Parse
import Tokenize
import qualified Data.Map as Map

type Environment = Map.Map String AST

evalAST env (CompoundNode (x:[])) = do
  a <- evalAST env x
  return a

evalAST env (CompoundNode ((ReturnNode n):xs)) = do
  (enva, a) <- evalAST env n
  return (enva, ReturnNode a)

evalAST env (CompoundNode (x:xs)) = do
  (envl, lv) <- evalAST env x
  let ll = case lv of ReturnNode n -> return (envl, n)
                      _ -> evalAST envl $ CompoundNode xs
  a <- ll
  return a

evalAST env (IntValueNode x) = do
  return (env, IntValueNode x)

evalAST env (BoolValueNode x) = do
  return (env, BoolValueNode x)

evalAST env (AssignNode (IdentNode name) r) = do
  (envr, IntValueNode rv) <- evalAST env r
  return (Map.insert name (IntValueNode rv) envr, IntValueNode rv)

evalAST env (IdentNode name) = do
  let Just value = Map.lookup name env
  return (env, value)

evalAST env (DefineFunctionNode name args ast) = do
  return (Map.insert name (FunctionNode name args ast) env, FunctionNode name args ast)

evalAST env (IfNode conditionAST thenAST elseAST) = do
  (_, BoolValueNode condition) <- evalAST env conditionAST
  x <- if condition then evalAST env thenAST else evalAST env elseAST
  return x

evalAST env (AddNode l r) = do
  (envl, IntValueNode lv) <- evalAST env  l
  (envr, IntValueNode rv) <- evalAST envl r
  return (envr, IntValueNode (lv + rv))

evalAST env (SubNode l r) = do
  (envl, IntValueNode lv) <- evalAST env  l
  (envr, IntValueNode rv) <- evalAST envl r
  return (envr, IntValueNode $ lv - rv)

evalAST env (MulNode l r) = do
  (envl, IntValueNode lv) <- evalAST env  l
  (envr, IntValueNode rv) <- evalAST envl r
  return (envr, IntValueNode $ lv * rv)

evalAST env (DivNode l r) = do
  (envl, IntValueNode lv) <- evalAST env  l
  (envr, IntValueNode rv) <- evalAST envl r
  return (envr, IntValueNode $ div lv rv)

evalAST env (EqNode l r) = do
  (envl, IntValueNode lv) <- evalAST env  l
  (envr, IntValueNode rv) <- evalAST envl r
  return (envr, BoolValueNode $ lv == rv)

evalAST env (SimpleNode l) = do
  x <- evalAST env l
  return x

evalAST env (CallNode "puts" (param:a)) = do
  (envv, v) <- evalAST env param
  print v
  return (envv, v)

evalAST env (CallNode name params) = do
  ps <- evalParams env params
  let Just (FunctionNode aaa args ast) = Map.lookup name env
      envLocal = (name, FunctionNode name args ast):(zipWith (\x y -> (x, y)) args ps)
  (e, x) <- evalAST (Map.fromList envLocal) ast
  let val = case x of ReturnNode y -> y
                      ys -> ys
  return (env, x)


evalAST env EmptyNode = do
  return (env, EmptyNode)

evalAST env (ReturnNode x) = do
  (e, val) <- evalAST env x
  return (e, ReturnNode val)

evalParams env [] = do
  return []

evalParams env (p:params) = do
  (envr, pp) <- evalAST env p
  ps <- evalParams env params
  return (pp:ps)

eval xs = do
  evalAST (Map.fromList []) $ parse xs
