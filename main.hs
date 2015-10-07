import Parse
import qualified Data.Map as Map

type Environment = Map.Map String Int

evalAST env (CompoundNode (x:[])) = do
  a <- evalAST env x
  return a

evalAST env (CompoundNode (x:xs)) = do
  (envl, lv) <- evalAST env x
  a <- evalAST envl $ CompoundNode xs
  return a

evalAST env (IntValueNode x) = do
  return (env, IntValueNode x)

evalAST env (AssignNode (IdentNode name) r) = do
  (envr, IntValueNode rv) <- evalAST env r
  return (Map.insert name rv envr, IntValueNode rv)

evalAST env (IdentNode name) = do
  let Just value = Map.lookup name env
  return (env, IntValueNode value)

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

evalAST env (SimpleNode l) = do
  x <- evalAST env l
  return x

eval xs = do
  evalAST (Map.fromList []) $ parse xs
