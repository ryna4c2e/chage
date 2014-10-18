-- AST（Abstract Syntax Tree）を定義する．
-- データ型の定義以外の役割を持たない．


module AST where

-- 32bitのデータを扱うため
import Data.Word


-- ポインタ変数を表すノード
data PtrVar = PtrVar String deriving (Show, Eq)

-- こちらは32bitの整数を表すノード
data IntVar = IntVar String deriving (Show, Eq)

-- 整数の値を利用している箇所を表すノード
data IntValue = Const Word32 
              | GetVar IntVar deriving (Show, Eq)

-- 簡単な式．
data SimpleExpr = Add IntValue IntValue
                | Mul IntValue IntValue

                | And IntValue IntValue
                | Xor IntValue IntValue
                | Or  IntValue IntValue


                | Cmpe  IntValue IntValue
                | Cmpne IntValue IntValue

                | Cmpl  IntValue IntValue
                | Cmple IntValue IntValue

                | Cmpg  IntValue IntValue
                | Cmpge IntValue IntValue

                | Load  PtrVar IntValue

                | Expr  IntValue
                  deriving (Show, Eq)

-- とりあえず，ASTは文の連続とする．
data AST = AST [Sentence] deriving (Show, Eq)

-- 文．
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
