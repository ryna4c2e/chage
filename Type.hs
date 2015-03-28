-- 型を定義する。

module Type where

data Type = S32Int
          | Pointer Type
            deriving (Show, Eq)
