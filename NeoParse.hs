import Tokenize

type TokenList = [Token]

data Program = Program StatementList
  deriving (Show)

data StatementList = StatementList [Statement]
  deriving (Show)

data Statement =
    IfStatement Expression StatementList StatementList
  | Expression Expression
  | EmptyStatement
  deriving (Show)

data Expression =
    Term Term
  | BinaryExpression String Term Expression
  deriving (Show)

data Term =
    Prime Prime
  | BinaryTerm String Prime Term
  deriving (Show)

data Prime =
    IntLiteral Int
  | StringLiteral String
  | BoolLiteral Bool
  | Parenthesis Expression
  | Ident String
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

parsePrime :: TokenList -> (Prime, TokenList)
parsePrime (IntToken n:tokens) = (IntLiteral n, tokens)
parsePrime (TrueToken:tokens) = (BoolLiteral True, tokens)
parsePrime (FalseToken:tokens) = (BoolLiteral False, tokens)
parsePrime (IdentToken ident:tokens) = (Ident ident, tokens)
parsePrime (ParenthesisBeginToken:tokens) = (Parenthesis expr, expredTokens)
  where (expr, (ParenthesisEndToken:expredTokens)) = parseExpression tokens
parsePrime _ = error "PrimeError"

parseTerm :: TokenList -> (Term, TokenList)
parseTerm tokens = case primedTokens of (MulOpToken:ts) -> (BinaryTerm "*" prime rterm, rtermedTokens)
                                        (DivOpToken:ts) -> (BinaryTerm "/" prime rterm, rtermedTokens)
                                        _ -> (Prime prime, primedTokens)
  where (prime, primedTokens) = parsePrime tokens
        (_:ts) = primedTokens
        (rterm, rtermedTokens) = parseTerm ts

parseExpression :: TokenList -> (Expression, TokenList)
parseExpression tokens = case termedTokens of AddOpToken:ts -> (BinaryExpression "+" term rexpr, rexpredTokens)
                                              SubOpToken:ts -> (BinaryExpression "-" term rexpr, rexpredTokens)
                                              _ -> (Term term, termedTokens)
  where (term, termedTokens) = parseTerm tokens
        (_:ts) = termedTokens
        (rexpr, rexpredTokens) = parseExpression ts

parseStatement :: TokenList -> (Statement, TokenList)
parseStatement (KeywordIfToken:tokens) = (IfStatement expr thenStmts elseStmts, elsedToken)
  where (expr, expredTokens) = parseExpression tokens
        ts = case expredTokens of (KeywordThenToken:NewLineToken:t) -> t
                                  (KeywordThenToken:t) -> t
                                  (NewLineToken:t) -> t
                                  t -> t
        (thenStmts, stmtsedTokens) = parseStatementList ts
        (tokenHead:tokenTail) = stmtsedTokens
        (elseStmts, elsedToken) = if tokenHead == KeywordElseToken then parseStatementList tokenTail else (StatementList [], stmtsedTokens)
parseStatement tokens = (Expression expr, expredTokens)
  where (expr, (NewLineToken:expredTokens)) = parseExpression tokens

parseStatementList :: TokenList -> (StatementList, TokenList)
parseStatementList [] = (StatementList [], [])
parseStatementList (NewLineToken:tokens) = parseStatementList tokens
parseStatementList (KeywordEndToken:tokens) = (StatementList [], tokens)
parseStatementList tokens@(KeywordElseToken:_) = (StatementList [], tokens)
parseStatementList tokens = (StatementList (stmt:stmts), ts)
  where (stmt, stmtedTokens) = parseStatement tokens
        (StatementList stmts, ts) = parseStatementList stmtedTokens

parseProgram :: TokenList -> (Program, TokenList)
parseProgram tokens = (Program stmts, ts)
  where (stmts, ts) = parseStatementList tokens

parse string = parseProgram $ tokenize string
