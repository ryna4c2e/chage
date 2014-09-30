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

-- 算術式の演算子．今は適当なデータ型を作った．
data ArithOperator = And
                   | Xor
                   | Or
                   | Add
                   | Mul
                     deriving (Show, Eq)
-- 比較の演算子．
data CompOperator = Cmpe
                  | Cmpne
                  | Cmpl
                  | Cmple
                  | Cmpg
                  | Cmpge
                    deriving (Show, Eq)
-- 簡単な式．
data Expr = Arith ArithOperator Expr Expr
          | Comp  CompOperator  Expr Expr

          | Load  PtrVar Expr
          | Expr  IntValue
            deriving (Show, Eq)

-- とりあえず，ASTは文の連続とする．
data AST = AST [Sentence] deriving (Show, Eq)

-- 文．
data Sentence = Assign IntVar Expr
              | If Expr AST AST
              | While Expr AST
              | DeclInt IntVar
              | DeclPtr PtrVar

              | PCopy PtrVar PtrVar
              | Store PtrVar IntValue Expr

              | Call String [Expr]

              | Data PtrVar [Word32]
              | Break
           deriving (Show, Eq)
