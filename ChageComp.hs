{-# LANGUAGE TemplateHaskell #-}

-- このソースコードは、次の記事を信頼して書かれました．
-- http://osecpu.osask.jp/wiki/?page0104
-- http://osecpu.osask.jp/wiki/?page0092


-- 変数のレジスタ割当は，レキシカルスコープのように，ある時点で使用されている変数を管理すれば良い．
-- ラベルの割当は，プログラム中でユニークであるべきなので，コンパイル中全体で管理する．

module ChageComp where

import Data.Word
import Data.List (genericLength)
import Control.Monad.State
import Control.Monad

-- Lensなる便利なライブラリを使うことにした
import Control.Lens 

import Inst
import AST 
import Type

-- 変数とレジスタの割当関係を管理する．
data Frame = Frame { _intVars :: [(Var, Reg)]
                   , _ptrVars :: [(Var, PReg)]
                   }

makeLenses ''Frame


emptyFrame :: Frame
emptyFrame = Frame [] []


-- コンパイル中に保持しておくべき情報．内部的に用いられるのみである．
data CompileState = CS { _labels :: Word32, -- 導入したラベルの数
                         _frame  :: Frame
                       }
makeLenses ''CompileState


-- 変数として確保できるレジスタの数の上限. [R00 ~ R27]
localRegisterCount = 0x27

-- テンポラリレジスタとして．[R28 ~ R2B]
tmp1 = Reg 0x3A
tmp2 = Reg 0x3B

-- 32bit指定のやつ．
spec32 = BitSpec 32

-- ポインタレジスタを使うとして，
p3f = PReg 0x3F
p30 = PReg 0x30
p3e = PReg 0x3E
p2f = PReg 0x2F

-- ラベルには二種類ある．ジャンプするためのラベルと，読み書きするためのラベルだ．
jumpOnly  = LabelOpt 0
readWrite = LabelOpt 1


-- 全て艦これが悪い
normalize :: AST -> AST
normalize ast = evalState (normalize' ast) 0
    where
      genSym :: State Int Var
      genSym = do st <- get
                  modify (1 +)
                  return $ Var $ "G" ++ show st
                         
      normalize' :: AST -> State Int AST
      normalize' (AST stmts) = do sss <- forM stmts normalizeSentence
                                  return $ AST (concat sss)

      normalizeExpr :: Expr -> Var -> State Int [Sentence]
      normalizeExpr expr var =
          case expr of
            Arith op e1 e2 ->
                do s1 <- genSym
                   s2 <- genSym
                   n1 <- normalizeExpr e1 s1
                   n2 <- normalizeExpr e2 s2
                   return $ n1 ++ n2 ++ [Declare var S32Int $ Arith op (GetVar s1) (GetVar s2)]
            Comp  op e1 e2 ->
                do s1 <- genSym
                   s2 <- genSym
                   n1 <- normalizeExpr e1 s1
                   n2 <- normalizeExpr e2 s2
                   return $ n1 ++ n2 ++ [Declare var S32Int $ Comp  op (GetVar s1) (GetVar s2)]
            ConstS32Int v ->
                do s <- genSym
                   return [Declare var S32Int $ ConstS32Int v]
            GetVar v ->
                do s <- genSym
                   return [Declare var S32Int $ GetVar v]
            Load ptr idx ->
                do s1 <- genSym
                   s2 <- genSym
                   n1 <- normalizeExpr ptr s1
                   n2 <- normalizeExpr idx s2
                   return $ n1 ++ n2 ++ [Declare var S32Int $ Load (GetVar s1) (GetVar s2)]

      normalizeSentence :: Sentence -> State Int [Sentence]
      normalizeSentence sentence =
          case sentence of
            Assign var expr -> normalizeExpr expr var
            If cnd csq alt  ->
                do s <- genSym
                   n1 <- normalizeExpr cnd s
                   a1 <- normalize' csq
                   a2 <- normalize' alt
                   return $ n1 ++ [If (GetVar s) a1 a2]
            While cnd body ->
                do s <- genSym
                   n <- normalizeExpr cnd s
                   a <- normalize' body
                   return $ n ++ [While (GetVar s) a]
            Declare var t expr -> normalizeExpr expr var
            Store dat idx val ->
                do s1 <- genSym
                   s2 <- genSym
                   s3 <- genSym
                   n1 <- normalizeExpr dat s1
                   n2 <- normalizeExpr idx s2
                   n3 <- normalizeExpr val s3
                   return $ n1 ++ n2 ++ n3 ++ [Store (GetVar s1) (GetVar s2) (GetVar s3)]
            Call name exprs ->
                do ss <- mapM (\_ -> genSym) exprs
                   ns <- sequence $ zipWith normalizeExpr exprs ss
                   return $ concat ns ++ [Call name (map GetVar ss)]
            Data var ws -> return [Data var ws]
            DebugStop -> return [DebugStop]

{-                         
compile :: AST -> Program
compile ast = Program $ evalState (compAST ast) (CS 0 emptyFrame)
    where
      compAST :: AST -> State CompileState [Inst]
      compAST (AST ss) = concat `fmap` mapM compSentence ss

      compSentence :: Sentence -> State CompileState [Inst]
      compSentence (DeclInt var) = defineVar var >> return []
      compSentence (DeclPtr ptr) = definePtr ptr >> return []

      compSentence (PCopy p0 p1) = do preg0 <- lookupPtr p0
                                      preg1 <- lookupPtr p1
                                      return $ [PCP preg0 preg1]

      compSentence (Store p ix expr) = do ptr <- lookupPtr p
                                          c1 <- compExprTo expr tmp1
                                          c2 <- compIntValTo ix tmp2

                                          return $ c1 ++ c2 ++ [PADD spec32 p3e ptr tmp2,
                                                                SMEM0 spec32 tmp1 p3e]

      compSentence (Data ptrvar ws) = do definePtr ptrvar
                                         pReg <- lookupPtr ptrvar
                                         lbl <- genLabel
                                         return $ [PLIMM pReg lbl, LB readWrite lbl, DATA ws]

      compSentence (Assign var expr) = do dst <- lookupVar var
                                          compExprTo expr dst

      compSentence (If cond cnsq altn) =
          do c1 <- compExprTo cond tmp1
             c2 <- extendScope (compAST cnsq)
             c3 <- extendScope (compAST altn)

             ifLabel <- genLabel
             endLabel  <- genLabel

             return $ c1 ++ [LIMM spec32 tmp2 (Imm 1), XOR spec32 tmp1 tmp1 tmp2,
                             CND tmp1, PLIMM p3f ifLabel] ++
                        c2 ++ [PLIMM p3f endLabel, LB jumpOnly ifLabel] ++
                        c3 ++ [LB jumpOnly endLabel]

      compSentence (While cond body) =
          do c1 <- compExprTo cond tmp1
             c2 <- extendScope (compAST body)

             startLabel <- genLabel
             endLabel <- genLabel

             return $ [LB jumpOnly startLabel] ++ c1 ++
                        [LIMM spec32 tmp2 (Imm 1), XOR spec32 tmp1 tmp1 tmp2,
                         CND tmp1, PLIMM p3f endLabel] ++
                          c2 ++ [PLIMM p3f startLabel, LB jumpOnly endLabel]

      compSentence (Call func args) =
          case lookup func apiList of
            Nothing -> error $ "function not found: " ++ func
            Just (inst, regs) -> do s <- sequence (zipWith compExprTo args regs)
                                    -- 逐次割り当てしておく。
                                    lb <- genLabel
                                    return $ concat s ++ [LIMM spec32 (Reg 0x30) inst,
                                                          PLIMM p30 lb,
                                                          PCP p3f p2f,
                                                          LB  readWrite lb]
      compSentence (Break) = return [BREAK]



      apiList = [("api_drawPoint", (Imm 0x0002, [Reg 0x31, Reg 0x32, Reg 0x33, Reg 0x34])),
                 ("api_sleep",     (Imm 0x0009, [Reg 0x31, Reg 0x32])),
                 ("api_openWin",   (Imm 0x0010, [Reg 0x31, Reg 0x32, Reg 0x33, Reg 0x34])),
                 ("api_fillOval",  (Imm 0x0005, [Reg 0x31, Reg 0x32, Reg 0x33, Reg 0x34, Reg 0x35, Reg 0x36]))]


      -- exprを、指定されたレジスタに計算していれる命令列を出す。
      compExprTo :: Expr -> Reg -> State CompileState [Inst]
      compExprTo (Expr v1)   outR = compIntValTo v1 outR


      compExprTo (Add v1 v2) outR = compArithmetic ADD v1 v2 outR
      compExprTo (Mul v1 v2) outR = compArithmetic MUL v1 v2 outR

      compExprTo (And v1 v2) outR = compArithmetic AND v1 v2 outR
      compExprTo (Xor v1 v2) outR = compArithmetic XOR v1 v2 outR
      compExprTo (Or  v1 v2) outR = compArithmetic OR  v1 v2 outR

      compExprTo (Cmpe  v1 v2) outR = compCompare CMPE  v1 v2 outR
      compExprTo (Cmpne v1 v2) outR = compCompare CMPNE v1 v2 outR
      compExprTo (Cmpl  v1 v2) outR = compCompare CMPL  v1 v2 outR
      compExprTo (Cmple v1 v2) outR = compCompare CMPLE v1 v2 outR
      compExprTo (Cmpg  v1 v2) outR = compCompare CMPG  v1 v2 outR
      compExprTo (Cmpge v1 v2) outR = compCompare CMPGE v1 v2 outR

      compExprTo (Load  ptr v) outR = do p <- lookupPtr ptr
                                         c2 <- compIntValTo v tmp1
                                         return $ c2 ++  [PADD spec32 p3e p tmp1, LMEM0 spec32 outR p3e]

      -- 簡単な算術命令について、compExprToをする。
      -- レジスタ同士の演算だと，すぐにできるため簡略化
      compArithmetic cons (GetVar v1) (GetVar v2) to = do
        vr1 <- lookupVar v1
        vr2 <- lookupVar v2
        return [cons spec32 to vr1 vr2]
      
      compArithmetic cons v1 v2 to = do c1 <- compIntValTo v1 tmp1
                                        c2 <- compIntValTo v2 tmp2
                                        return $ (c1 ++ c2 ++ [cons spec32 to tmp1 tmp2])

      -- 簡単な比較命令について、compExprToをする。
      compCompare cons = compArithmetic (cons spec32)

      
      compIntValTo (AST.Const word) reg = return [LIMM spec32 reg (Imm word)]
      compIntValTo (GetVar v) reg = do varReg <- lookupVar v
                                       return [OR spec32 reg varReg varReg]

      
      -- スコープのネストした中でモナドを走らせる．要は，いったん戻してるだけ．
      extendScope :: State CompileState a -> State CompileState a
      extendScope m = do
        fr <- use frame
        ret <- m
        frame .= fr
        return ret

      -- 整数の変数を，シンボルテーブルから引っ張ってくる．
      lookupVar :: IntVar -> State CompileState Reg
      lookupVar var@(IntVar s) = do
        vars <- use (frame.intVars)
        case lookup var vars of
          Nothing -> error $ "undefined variable: " ++ s
          Just rg -> return rg

      -- ポインタ変数を，シンボルテーブルから引っ張ってくる．
      lookupPtr :: PtrVar -> State CompileState PReg
      lookupPtr ptr@(PtrVar s) = do
        vars <- use (frame.ptrVars)
        case lookup ptr vars of
          Nothing -> error $ "undefined pointer variable: " ++ s
          Just pr -> return pr

      defineVar var@(IntVar _) = do
        reg <- use (frame.intVars) <&> genericLength <&> Reg
        frame.intVars %= ((var, reg) :)
        return $ reg

      definePtr ptr@(PtrVar _) = do
        reg <- use (frame.ptrVars) <&> genericLength <&> PReg
        frame.ptrVars %= ((ptr, reg) :)
        return $ reg

      genLabel = do
        lb <- use labels
        labels += 1
        return $ Label lb

-}


test1 = AST [
         Declare (Var "x") S32Int (Arith Add
                                   (Arith Xor (ConstS32Int 3) (ConstS32Int 4))
                                   (ConstS32Int 5))]
test2 = AST [
         Data  (Var "d") [1, 2, 3],
         Declare (Var "y") S32Int (Load (GetVar (Var "d")) (ConstS32Int 1)),
         Store (GetVar (Var "d")) (ConstS32Int 3) (ConstS32Int 4)
        ]
