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
         | IntLitNode Int
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
