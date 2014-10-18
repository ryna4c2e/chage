{- IR

ASTから命令列に変換する行程を，
1，名前解決
2，命令列への変換

の2つにわけてしまおうと考えた．
そのため，このモジュールでは名前解決が済んだあと，構造を保った形でのプログラムというものを定義することとした．


-}

module IR where

-- 定数
newtype Const = Const Word32

-- 変数は背番号で．
newtype IVar = IVar Int
newtype PVar = PVar Int

data IR = UnaryOp  UnaryOperator  IVar IVar
        | BinaryOp BinaryOperator IVar IVar IVar
        | 
              
