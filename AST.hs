-- AST（Abstract Syntax Tree）を定義する．
-- データ型の定義以外の役割を持たない．


module AST where

import Type

-- 32bitのデータを扱うため
import Data.Word


-- 変数名を表すノード
newtype Var = Var String deriving (Show, Eq)

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
-- 式．
data Expr t = Arith t ArithOperator Expr Expr -- $1 op   $2
            | Comp  t CompOperator  Expr Expr -- $1 comp $2
            | ConstS32Int t Int
            | GetVar t Var
            | Load t Expr Expr -- get value of $1[$2]
            deriving (Show, Eq)

-- とりあえず，ASTは文の連続とする．
newtype AST t = AST [Sentence t] deriving (Eq)

instance Show t => Show (AST t) where
    show (AST stmts) = unlines $ map show stmts
    
-- 文．
data Sentence t = Assign  Var  (Expr t)
                | If      (Expr t) (AST t) (AST t)
                | While   (Expr t) (AST t)
                | Declare Var Type (Expr t)

                | Store   (Expr t) (Expr t) (Expr t) -- store $3 into $1[$2]
 
                | Call    String [Expr t]

                | Data    Var [Word32]
                | DebugStop
                  deriving (Show, Eq)
