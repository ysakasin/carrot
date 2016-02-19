module Parser
  (
    parse
  ) where
import Control.Applicative((<|>))
import Control.Monad (forM_)
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Text.Trifecta
import Text.Trifecta.Delta(Delta(..))
import Text.Parser.Token
import Text.Parser.Token.Highlight(Highlight(..))
import AST

program :: Parser AST
program = do
  e <- statements
  eof
  return e

statements :: Parser AST
statements = do
  stmts <- many statement
  return $ CompoundNode stmts

statement :: Parser AST
statement = ifStatement <|> defineFunctionStatement <|> returnStatement<|> expression

ifStatement :: Parser AST
ifStatement = do
  symbol "if"
  condition <- expression
  symbol "then"
  thenStmts <- statements
  elseStmts <- optional $ do
    s <- elsifStatement <|> elseStatement
    -- s <- elsifStatement
    return s
  symbol "end"
  case elseStmts of
    Nothing -> return $ IfNode condition thenStmts EmptyNode
    Just es -> return $ IfNode condition thenStmts es

elsifStatement :: Parser AST
elsifStatement = do
  symbol "elsif"
  condition <- expression
  symbol "then"
  thenStmts <- statements
  elseStmts <- optional $ do
    s <- elsifStatement <|> elseStatement
    -- s <- elseStatement
    return s
  case elseStmts of
    Nothing -> return $ IfNode condition thenStmts EmptyNode
    Just es -> return $ IfNode condition thenStmts es

elseStatement :: Parser AST
elseStatement = do
  symbol "else"
  stmts <- statements
  return stmts

defineFunctionStatement :: Parser AST
defineFunctionStatement = do
  symbol "def"
  name <- identString
  spaces
  symbol "("
  args <- identString `sepBy` symbol ","
  symbol ")"
  stmts <- statements
  symbol "end"
  return $ DefineFunctionNode name args stmts

returnStatement = do
  symbol "return"
  e <- expression
  return $ ReturnNode e

eqop  = do{ symbol "=="; return EqNode } <|> do{ symbol "="; return AssignNode }
cmpop = do{ symbol ">="; return AndLessNode } <|> do{ symbol "<="; return AndMoreNode }
        <|> do{ symbol ">"; return LessNode } <|> do{ symbol "<"; return MoreNode }
mulop = do{ symbol "*"; return MulNode } <|> do{ symbol "/"; return DivNode }
addop = do{ symbol "+"; return AddNode } <|> do{ symbol "-"; return SubNode }

-- expression = eqexpr <|> assignExpr
expression = eqexpr
eqexpr  = cmpexpr `chainl1` eqop
cmpexpr = expr   `chainl1` cmpop
expr    = term   `chainl1` addop
term    = factor `chainl1` mulop
factor  = parens expr <|> intLiteral <|> callExpression

assignExpr = do
  assignee <- try $ do {a <- identLiteral; symbol "="; return a}
  e <- expression
  return $ AssignNode assignee e

callExpression = do
  s <- identString
  args <- optional $ do
    parens $ expr `sepBy` symbol ","
  case args of
    Nothing -> return $ IdentNode s
    Just argExprs -> return $ CallNode s argExprs

intLiteral = do
  n <- integer
  return $ IntLitNode n

identLiteral = do
  s <- identString
  return $ IdentNode s

identString = ident carrotIdents

carrotIdents :: TokenParsing m => IdentifierStyle m
carrotIdents = IdentifierStyle
  { _styleName     = "identifier"
  , _styleStart    = letter <|> char '_'
  , _styleLetter   = alphaNum <|> oneOf "_'"
  , _styleReserved = set ["if", "elsif", "else", "then", "end", "do", "def", "return"]
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

set :: [String] -> HashSet String
set = HashSet.fromList

parse str = parseString program (Columns 0 0) str
