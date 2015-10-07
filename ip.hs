import qualified Data.Map as Map

data Expression = ValueInt Int
                | Operator String Expression Expression
                | Function String [Expression]
                | Expr Expression
                | Name String
                | Compound [Expression]
                | Empty
  deriving (Show)

type Env = Map.Map String Expression

genSyntaxTree :: [String] -> Expression -> Expression
genSyntaxTree [] expr = expr
genSyntaxTree ("=":xs) expr = Operator "=" (genSyntaxTree xs Empty) expr
genSyntaxTree ("+":xs) expr = Operator "+" (genSyntaxTree xs Empty) expr
genSyntaxTree ("*":xs) expr = Operator "*" (genSyntaxTree xs Empty) expr
genSyntaxTree ("puts":xs) expr = Function "puts" [expr]
genSyntaxTree (")":xs) expr = genSyntaxTree outer $ genSyntaxTree inner expr
  where (outerr, "(":innerr) = break ("(" ==) $ reverse xs
        outer = reverse outerr
        inner = reverse innerr
genSyntaxTree (x:xs) expr
  | foldl (\acc y -> acc && elem y ['0'..'9']) True x = genSyntaxTree xs $ ValueInt (read x::Int)
  | otherwise = genSyntaxTree xs $ Name x

parce :: String -> [Expression]
parce xs = map (\x -> genSyntaxTree x Empty) $ map (\x -> reverse (words x)) $ lines xs


evalSyntaxTree env (ValueInt x) = do
  return (env, ValueInt x)

evalSyntaxTree env (Operator "+" l r) = do
  (envl, ValueInt lv) <- evalSyntaxTree env l
  (envr, ValueInt rv) <- evalSyntaxTree envl r
  return (envr, ValueInt $ lv + rv)

evalSyntaxTree env (Operator "=" (Name name) r) = do
  (envr, er) <- evalSyntaxTree env r
  return (Map.insert name er envr, er)

evalSyntaxTree env (Function "puts" [x]) = do
  (envr, r) <- evalSyntaxTree env x
  print r
  return (env, r)

evalSyntaxTree env (Name name) = do
  return (env, toExpression $ Map.lookup name env)

evalSyntaxTree env (Compound (expr:[])) = do
  a <- evalSyntaxTree env expr
  return a

evalSyntaxTree env (Compound (expr:es)) = do
  (envr, _) <- evalSyntaxTree env expr
  (envrr, _) <- evalSyntaxTree envr $ Compound es
  return (envrr, Empty)


toExpression Nothing = Empty
toExpression (Just x) = x

eval xs = do
  evalSyntaxTree (Map.fromList []) $ Compound $ parce xs
