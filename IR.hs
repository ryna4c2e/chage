-- 中間的な表現について、a + b * cのような式を、
-- tmp = b * c; a * tmp
-- と変換したり。

module IR where

import Data.Word
import Control.Monad.State
import Type
import qualified AST as A
    
data IR = If      A.Var [IR] [IR] -- if var consequence alternative
        | While   A.Var [IR] [IR] -- while var condition body
        | Declare A.Var Type A.Var
        | Arith   A.ArithOperator A.Var A.Var A.Var
        | Comp    A.CompOperator  A.Var A.Var A.Var
        | ConstS32Int A.Var Int
        | GetVar  A.Var Type A.Var
        | Load    A.Var A.Var A.Var
        | Store   A.Var A.Var A.Var
        | Call    String [A.Var]
        | Data    A.Var [Word32]
        | DebugStop
          deriving (Show, Eq)


normalize :: A.AST Type -> [IR]
normalize ast = evalState (normalize' ast) 0
    where
      genSym :: State Int A.Var
      genSym = do st <- get
                  modify (1 +)
                  return $ A.Var $ "G" ++ show st

      normalize' :: A.AST Type -> State Int [IR]
      normalize' (A.AST stmts) = do irs <- forM stmts normalizeSentence
                                    return (concat irs)
                                         
      normalizeSentence :: A.Sentence Type -> State Int [IR]
      normalizeSentence s =
          case s of
            A.Assign var expr -> normalizeExpr expr var
            A.If expr csq alt -> do (s, i1) <- assignToNewVar expr
                                    i2 <- normalize' csq
                                    i3 <- normalize' alt
                                    return $ i1 ++ [If s i2 i3]
            A.While expr body -> do (s, i1) <- assignToNewVar expr
                                    i2 <- normalize' body
                                    return [While s i1 i2]
            A.Declare var t v -> do (s1, i1) <- assignToNewVar v
                                    return $ i1 ++ [Declare var t s1]
            A.Store dat idx v -> do (s1, i1) <- assignToNewVar dat
                                    (s2, i2) <- assignToNewVar idx
                                    (s3, i3) <- assignToNewVar v
                                    return $ i1 ++ i2 ++ i3 ++ [Store s1 s2 s3]
            A.Call name exprs -> do args <- mapM assignToNewVar exprs
                                    return $ concat (map snd args) ++ [Call name (map fst args)]
            A.Data var ws -> return [Data var ws]
            A.DebugStop -> return [DebugStop]
                           
      normalizeExpr :: A.Expr Type -> A.Var -> State Int [IR]
      normalizeExpr expr var =
          case expr of
            A.Arith t op e1 e2 -> do (s1, i1) <- assignToNewVar e1
                                     (s2, i2) <- assignToNewVar e2
                                     return $ i1 ++ i2 ++ [Arith op var s1 s2]
            A.Comp  t op e1 e2 -> do (s1, i1) <- assignToNewVar e1
                                     (s2, i2) <- assignToNewVar e2
                                     return $ i1 ++ i2 ++ [Comp op var s1 s2]
            A.ConstS32Int t v  -> return [ConstS32Int var v]
            A.GetVar t v       -> return [Declare var t v]
            A.Load t e1 e2     -> do (s1, i1) <- assignToNewVar e1
                                     (s2, i2) <- assignToNewVar e2
                                     return $ i1 ++ i2 ++ [Load var s1 s2]

      assignToNewVar :: A.Expr Type -> State Int (A.Var, [IR])
      assignToNewVar expr = do s <- genSym
                               n <- normalizeExpr expr s
                               return (s, n)
