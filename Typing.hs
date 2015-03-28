-- 肩検査のモジュール

module Typing where

import Control.Applicative    
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Control.Lens
    
import Type
import AST

type TypeEnv = [(Var, Type)]
type TypingM = StateT TypeEnv (Either String)

typing :: AST -> AST
typing ast = case evalStateT (typing' ast) [] of
               Right ()   -> ast
               Left  err' -> error err'
               

typing' :: AST -> TypingM ()
typing' (AST sentences) = forM_ sentences typingSentence
    where
      typingSentence :: Sentence -> TypingM ()
      typingSentence sentence = 
          case sentence of
            Assign var expr -> do t1 <- typeLookup var
                                  t2 <- typeOf expr
                                  typeAssert t1 t2
            If cond csq alt -> do t1 <- typeOf cond
                                  typeAssert S32Int t1
                                  typing' csq
                                  typing' alt
            While cond body -> do t1 <- typeOf cond
                                  typeAssert S32Int t1
                                  typing' body
            Declare v t ini -> do t1 <- typeOf ini
                                  typeAssert t t1
                                  putVar v t

            Store dat idx v -> do t1 <- typeOf dat
                                  t2 <- typeOf idx
                                  t3 <- typeOf v
                                  case (t1, t2, t3) of
                                    (Pointer t, S32Int, s) | s == t -> return ()
                                    otherwise -> lift (throwError "store mismatch")
            Call name exprs -> forM_ exprs $ \e -> do
                                 t1 <- typeOf e
                                 typeAssert S32Int t1
            Data v _  -> putVar v (Pointer S32Int)
            DebugStop -> return ()

putVar :: Var -> Type -> TypingM ()
putVar v t = modify ((v, t):)
typeLookup :: Var -> TypingM Type
typeLookup var = do st <- get
                    case lookup var st of
                      Just x  -> return x
                      Nothing -> throwError ("var not found in type checking" ++ show var)
                                         
typeOf :: Expr -> TypingM Type
typeOf expr = case expr of
                Arith _ e1 e2 -> do t1 <- typeOf e1
                                    t2 <- typeOf e2
                                    case (t1, t2) of
                                      (S32Int, S32Int) -> return S32Int
                                      otherwise -> lift (throwError "arithmetic operand error")
                Comp  _ e1 e2 -> do t1 <- typeOf e1
                                    t2 <- typeOf e2
                                    case (t1, t2) of
                                      (S32Int, S32Int) -> return S32Int
                                      otherwise -> lift (throwError "comparison operand error")
                Load dat idx  -> do t1 <- typeOf dat
                                    t2 <- typeOf idx
                                    case (t1, t2) of
                                      (Pointer x, y) | x == y -> return x
                                      otherwise -> lift (throwError "indexing operation error")
                ConstS32Int int -> return S32Int
                GetVar var -> typeLookup var
                                              
typeAssert :: Type -> Type -> TypingM ()
typeAssert t1 t2 | t1 /= t2  = lift (throwError ("type mismatch: " ++ show t1 ++ " != " ++ show t2))
                 | otherwise = return ()



testAST1 = AST [
           Declare (Var "x") S32Int (ConstS32Int 0),
           Data (Var "y") [1, 2, 3],
           While (Comp Cmpe (GetVar (Var "x")) (GetVar (Var "y"))) (AST [DebugStop])
           
           ]
