module AST where

import Data.Word




data IntVar = IntVar String deriving (Show, Eq)

data IntValue = Imm Word32 
              | GetVar IntVar deriving (Show, Eq)


data SimpleExpr = Add IntValue IntValue
                | Mul IntValue IntValue

                | Cmpe  IntValue IntValue
                | Cmpne IntValue IntValue

                | Cmpl  IntValue IntValue
                | Cmple IntValue IntValue

                | Cmpg  IntValue IntValue
                | Cmpge IntValue IntValue
                | Expr  IntValue
                  deriving (Show, Eq)

data AST = AST [Sentence] deriving (Show, Eq)

data Sentence = Assign IntVar SimpleExpr
              | If SimpleExpr AST AST
              | While SimpleExpr AST
              | Decl IntVar
           deriving (Show, Eq)
