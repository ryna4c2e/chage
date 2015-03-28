-- 型を定義する。

module Type where

data Type = SInt32
          | Pointer Type
            deriving (Show, Eq)
