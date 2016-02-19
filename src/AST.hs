module AST
(
  AST(..)
) where

data AST = SimpleNode AST
         | AssignNode AST AST
         | AddNode AST AST
         | SubNode AST AST
         | MulNode AST AST
         | DivNode AST AST
         | EqNode AST AST
         | LessNode AST AST
         | MoreNode AST AST
         | AndLessNode AST AST
         | AndMoreNode AST AST
         | IntLitNode Integer
         | BoolLitNode Bool
         | IdentNode String
         | CompoundNode [AST]
         | DefineFunctionNode String [String] AST
         | FunctionNode String [String] AST
         | IfNode AST AST AST
         | EmptyNode
         | CallNode String [AST]
         | ReturnNode AST
  deriving (Show)
