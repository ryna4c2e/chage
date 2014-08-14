module AST where

import Data.Word



data PtrVar = PtrVar String deriving (Show, Eq)
data IntVar = IntVar String deriving (Show, Eq)

data IntValue = Const Word32 
              | GetVar IntVar deriving (Show, Eq)


data SimpleExpr = Add IntValue IntValue
                | Mul IntValue IntValue

                | Cmpe  IntValue IntValue
                | Cmpne IntValue IntValue

                | Cmpl  IntValue IntValue
                | Cmple IntValue IntValue

                | Cmpg  IntValue IntValue
                | Cmpge IntValue IntValue

                | Load  PtrVar IntValue

                | Expr  IntValue
                  deriving (Show, Eq)

data AST = AST [Sentence] deriving (Show, Eq)

data Sentence = Assign IntVar SimpleExpr
              | If SimpleExpr AST AST
              | While SimpleExpr AST
              | DeclInt IntVar
              | DeclPtr PtrVar

              | PCopy PtrVar PtrVar
              | Store PtrVar IntValue SimpleExpr

              | Call String [SimpleExpr]

              | Data PtrVar [Word32]
              | Break
           deriving (Show, Eq)
