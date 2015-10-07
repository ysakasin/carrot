import qualified Data.Map as Map

data Token = IntToken Int
           | ValueToken String
           | ParenthesisBeginToken
           | ParenthesisEndToken
           | DoubleQuotationToken
           | AssignOpToken
           | AddOpToken
           | SubOpToken
           | MulOpToken
           | DivOpToken
           | CommaToken
           | EmptyToken
           | FunctionToken String
           | ArgsToken [String]
           | PrimaryToken [Token]
  deriving (Show, Eq)

data PrimaryExpression = PrimaryInt Int
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
         | AddNode AST AST
         | SubNode AST AST
         | MulNode AST AST
         | DivNode AST AST
         | IntValueNode Int
  deriving (Show)


tokenize [] = []
tokenize ("(":xs)  = ParenthesisBeginToken:tokenize(xs)
tokenize (")":xs)  = ParenthesisEndToken:tokenize(xs)
tokenize ("\"":xs) = DoubleQuotationToken:tokenize(xs)
tokenize ("=":xs)  = AssignOpToken:tokenize(xs)
tokenize ("+":xs)  = AddOpToken:tokenize(xs)
tokenize ("-":xs)  = SubOpToken:tokenize(xs)
tokenize ("*":xs)  = MulOpToken:tokenize(xs)
tokenize ("/":xs)  = DivOpToken:tokenize(xs)
tokenize (",":xs)  = CommaToken:tokenize(xs)
tokenize (x:xs)
  | foldl (\acc y -> acc && elem y ['0'..'9']) True x = IntToken (read x::Int):tokenize(xs)
  | otherwise = ValueToken x:tokenize(xs)

tokenizePrimaryToken :: [Token] -> [Token]
tokenizePrimaryToken (ParenthesisBeginToken:ts) = (PrimaryToken inner):(tokenizePrimaryToken outer)
  where (router, ParenthesisEndToken:rinner) = break (ParenthesisEndToken ==) $ reverse ts
        outer = reverse router
        inner = reverse rinner
tokenizePrimaryToken (t:[]) = [t]
tokenizePrimaryToken (t:ts) = t:(tokenizePrimaryToken ts)

parseExpression :: [Token] -> AST
parseExpression vs
  | not $ avr == [] = case x of AddOpToken -> AddNode (parseTerm avl) $ parseExpression navr
                                SubOpToken -> SubNode (parseTerm avl) $ parseExpression navr
  | otherwise = parseTerm vs
  where (avl, avr) = break (\x -> AddOpToken == x || SubOpToken == x) vs
        x:navr = avr

parseTerm :: [Token] -> AST
parseTerm ts
  | not $ avr == [] = case x of MulOpToken -> MulNode (parsePrimaryExpression avl) $ parseTerm navr
                                SubOpToken -> DivNode (parsePrimaryExpression avl) $ parseTerm navr
  | otherwise = parsePrimaryExpression ts
  where (avl, avr) = break (\x -> MulOpToken == x || DivOpToken == x) ts
        x:navr = avr

parsePrimaryExpression :: [Token] -> AST
parsePrimaryExpression [IntToken x] = IntValueNode x
parsePrimaryExpression [PrimaryToken x] = parseExpression x
parsePrimaryExpression _ = error "Parse Error"


type Environment = Map.Map String Expression

evalAST env (IntValueNode x) = do
  return (env, IntValueNode x)

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


parse :: String -> AST
parse xs = parseExpression $ tokenizePrimaryToken $ tokenize $ words xs

check xs = tokenizePrimaryToken $ tokenize $ words xs

eval xs = do
  evalAST (Map.fromList []) $ parse xs
