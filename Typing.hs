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

typing :: AST () -> AST Type
typing ast = case evalStateT (typing' ast) [] of
               Right ast' -> ast'
               Left  err' -> error err'
               

typing' :: AST t -> TypingM (AST Type)
typing' (AST sentences) = AST <$> mapM typingSentence sentences
    where
      typingSentence :: Sentence t -> TypingM (Sentence Type)
      typingSentence sentence = 
          case sentence of
            Assign var expr -> do t1 <- typeLookup var
                                  e2 <- typingExpr expr
                                  typeAssert t1 (typeOf e2)
                                  return $ Assign var e2
            If cond csq alt -> do e1 <- typingExpr cond
                                  typeAssert S32Int (typeOf e1)
                                  a1 <- typing' csq
                                  a2 <- typing' alt
                                  return $ If e1 a1 a2
            While cond body -> do e1 <- typingExpr cond
                                  typeAssert S32Int (typeOf e1)
                                  a1 <- typing' body
                                  return $ While e1 a1
            Declare v t ini -> do e1 <- typingExpr ini
                                  typeAssert t (typeOf e1)
                                  putVar v t
                                  return $ Declare v t e1
            Store dat idx v -> do e1 <- typingExpr dat
                                  e2 <- typingExpr idx
                                  e3 <- typingExpr v
                                  case (typeOf e1, typeOf e2, typeOf e3) of
                                    (Pointer t, S32Int, s) | s == t -> return $ Store e1 e2 e3
                                    otherwise -> lift (throwError "store mismatch")
            Call name exprs -> do args <- forM exprs $ \e -> do
                                            e' <- typingExpr e
                                            typeAssert S32Int (typeOf e')
                                            return e'
                                  return $ Call name args
            Data v d  -> do putVar v (Pointer S32Int)
                            return $ Data v d
            DebugStop -> return DebugStop


putVar :: Var -> Type -> TypingM ()
putVar v t = modify ((v, t):)
             
typeLookup :: Var -> TypingM Type
typeLookup var = do st <- get
                    case lookup var st of
                      Just x  -> return x
                      Nothing -> throwError ("var not found in type checking" ++ show var)
                                         
typingExpr :: Expr t -> TypingM (Expr Type)
typingExpr expr =
    case expr of
      Arith _ o e1 e2 -> do t1 <- typingExpr e1
                            t2 <- typingExpr e2
                            case (typeOf t1, typeOf t2) of
                              (S32Int, S32Int) -> return $ Arith S32Int o t1 t2
                              otherwise -> lift (throwError "arithmetic operand error")
      Comp _ o e1 e2 -> do t1 <- typingExpr e1
                           t2 <- typingExpr e2
                           case (typeOf t1, typeOf t2) of
                             (S32Int, S32Int) -> return $ Comp S32Int o t1 t2
                             otherwise -> lift (throwError "comparison operand error")
      Load _ dat idx  -> do t1 <- typingExpr dat
                            t2 <- typingExpr idx
                            case (typeOf t1, typeOf t2) of
                              (Pointer x, y) | x == y -> return $ Load y t1 t2
                              otherwise -> lift (throwError "indexing operation error")
      ConstS32Int _ int -> return $ ConstS32Int S32Int int
      GetVar _ var -> do t <- typeLookup var
                         return $ GetVar t var


typeOf expr =
    case expr of
      Arith t _ _ _ -> t
      Comp  t _ _ _ -> t
      ConstS32Int t _ -> t
      GetVar t _      -> t
      Load t _ _ -> t
                   
typeAssert :: Type -> Type -> TypingM ()
typeAssert t1 t2 | t1 /= t2  = lift (throwError ("type mismatch: " ++ show t1 ++ " != " ++ show t2))
                 | otherwise = return ()



testAST1 = AST [
           Declare (Var "x") S32Int (ConstS32Int () 0),
           Data (Var "y") [1, 2, 3],
           While (Load () (GetVar () (Var "y")) (GetVar () (Var "x"))) (AST [DebugStop])
           
           ]
