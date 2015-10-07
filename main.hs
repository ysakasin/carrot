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

parseExpression :: [Token] -> Expression
parseExpression vs
  | not $ avr == [] = case x of AddOpToken -> AddExpression (parseTerm avl) $ parseExpression navr
                                SubOpToken -> SubExpression (parseTerm avl) $ parseExpression navr
  | otherwise = SimpleExpression $ parseTerm vs
  where (avl, avr) = break (\x -> AddOpToken == x || SubOpToken == x) vs
        x:navr = avr

parseTerm :: [Token] -> Term
parseTerm ts
  | not $ avr == [] = case x of MulOpToken -> MulTerm (parsePrimaryExpression avl) $ parseTerm navr
                                SubOpToken -> DivTerm (parsePrimaryExpression avl) $ parseTerm navr
  | otherwise = SimpleTerm $ parsePrimaryExpression ts
  where (avl, avr) = break (\x -> MulOpToken == x || DivOpToken == x) ts
        x:navr = avr

parsePrimaryExpression :: [Token] -> PrimaryExpression
parsePrimaryExpression [IntToken x] = PrimaryInt x
parsePrimaryExpression [PrimaryToken x] = PrimaryParenthesis $ parseExpression x
parsePrimaryExpression _ = error "Parse Error"

type Environment = Map.Map String Expression

evalExpression :: Environment -> Expression -> (Environment, 
evalExpression _ (SimpleExpression SimpleTerm PrimaryInt x)

parse xs = parseExpression $ tokenizePrimaryToken $ tokenize $ words xs

check xs = tokenizePrimaryToken $ tokenize $ words xs
