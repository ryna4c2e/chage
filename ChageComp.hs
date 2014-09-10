-- このソースコードは、次の記事を信頼して書かれました．
-- http://osecpu.osask.jp/wiki/?page0104
-- http://osecpu.osask.jp/wiki/?page0092


module ChageComp where

import Data.Word
import Control.Monad.State
import Control.Monad


import Inst
import AST



-- コンパイル中に保持しておくべき情報．内部的に用いられるのみである．
data CompileState = CS { registers :: Word32, -- 使用した整数レジスタの数
                         labels :: Word32, -- 導入したラベルの数
                         ptrRegisters :: Word32, -- 使用したポインタレジスタの数
                         intVars :: [(IntVar, Reg)],　-- 変数とレジスタの対応
                         ptrVars :: [(PtrVar, PReg)] -- ポインタ変数とレジスタの対応
                       }

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


compile :: AST -> Program
compile ast = Program $ evalState (compAST ast) (CS 0 0 1 [] [])
    where
      compAST :: AST -> State CompileState [Inst]
      compAST (AST ss) = do words <- mapM compSentence ss
                            return $ concat words

      compSentence :: Sentence -> State CompileState [Inst]
      compSentence (DeclInt var) = defineVar var >> return []
      compSentence (DeclPtr ptr) = definePtr ptr >> return []

      compSentence (PCopy p0 p1) = do preg0 <- lookupPtr p0
                                      preg1 <- lookupPtr p1
                                      return $ [PCP preg0 preg1]

      compSentence (Store p ix expr) = do ptr <- lookupPtr p
                                          c1 <- compSimpleExprTo expr tmp1
                                          c2 <- compIntValTo ix tmp2

                                          return $ c1 ++ c2 ++ [PADD spec32 p3e ptr tmp2,
                                                                SMEM0 spec32 tmp1 p3e]

      compSentence (Data ptrvar ws) = do definePtr ptrvar
                                         pReg <- lookupPtr ptrvar
                                         lbl <- genLabel
                                         return $ [PLIMM pReg lbl, LB readWrite lbl, DATA ws]

      compSentence (Assign var expr) = do c1 <- compSimpleExprTo expr tmp1 
                                          dst <- lookupVar var
                                          return $ c1 ++ [OR spec32 dst tmp1 tmp1]

      compSentence (If cond cnsq altn) =
          do c1 <- compSimpleExprTo cond tmp1
             c2 <- compAST cnsq
             c3 <- compAST altn

             ifLabel <- genLabel
             endLabel  <- genLabel
             
             return $ c1 ++ [LIMM spec32 tmp2 (Imm 1), XOR spec32 tmp1 tmp1 tmp2,
                             CND tmp1, PLIMM p3f ifLabel] ++
                        c2 ++ [PLIMM p3f endLabel, LB jumpOnly ifLabel] ++
                        c3 ++ [LB jumpOnly endLabel]

      compSentence (While cond body) =
          do c1 <- compSimpleExprTo cond tmp1
             c2 <- compAST body

             startLabel <- genLabel
             endLabel <- genLabel

             return $ [LB jumpOnly startLabel] ++ c1 ++
                        [LIMM spec32 tmp2 (Imm 1), XOR spec32 tmp1 tmp1 tmp2,
                         CND tmp1, PLIMM p3f endLabel] ++
                          c2 ++ [PLIMM p3f startLabel, LB jumpOnly endLabel]

      compSentence (Call func args) =
          case lookup func apiList of 
            Nothing -> error $ "function not found: " ++ func
            Just (inst, regs) -> do s <- sequence (zipWith compSimpleExprTo args regs)
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
       

      -- simplexpr を、指定されたレジスタに計算していれる命令列を出す。
      compSimpleExprTo :: SimpleExpr -> Reg -> State CompileState [Inst]
      compSimpleExprTo (Expr v1)   outR = compIntValTo v1 outR
      compSimpleExprTo (Add v1 v2) outR = compArithmetic ADD v1 v2 outR
      compSimpleExprTo (Mul v1 v2) outR = compArithmetic MUL v1 v2 outR

      compSimpleExprTo (And v1 v2) outR = compArithmetic AND v1 v2 outR
      compSimpleExprTo (Xor v1 v2) outR = compArithmetic XOR v1 v2 outR
      compSimpleExprTo (Or  v1 v2) outR = compArithmetic OR  v1 v2 outR

      compSimpleExprTo (Cmpe  v1 v2) outR = compCompare CMPE  v1 v2 outR
      compSimpleExprTo (Cmpne v1 v2) outR = compCompare CMPNE v1 v2 outR
      compSimpleExprTo (Cmpl  v1 v2) outR = compCompare CMPL  v1 v2 outR
      compSimpleExprTo (Cmple v1 v2) outR = compCompare CMPLE v1 v2 outR
      compSimpleExprTo (Cmpg  v1 v2) outR = compCompare CMPG  v1 v2 outR
      compSimpleExprTo (Cmpge v1 v2) outR = compCompare CMPGE v1 v2 outR

      compSimpleExprTo (Load  ptr v) outR = do p <- lookupPtr ptr
                                               c2 <- compIntValTo v tmp1
                                               return $ c2 ++  [PADD spec32 p3e p tmp1, LMEM0 spec32 outR p3e]

      -- 簡単な算術命令について、compSimpleExprToをする。
      compArithmetic cons v1 v2 to = do c1 <- compIntValTo v1 tmp1
                                        c2 <- compIntValTo v2 tmp2
                                        return $ (c1 ++ c2 ++ [cons spec32 to tmp1 tmp2])

      -- 簡単な比較命令について、compSimpleExprToをする。
      compCompare cons = compArithmetic (cons spec32)


      compIntValTo (Const word) reg = return [LIMM spec32 reg (Imm word)]
      compIntValTo (GetVar v) reg = do varReg <- lookupVar v
                                       return [OR spec32 reg varReg varReg]

      -- 整数の変数を，シンボルテーブルから引っ張ってくる．
      lookupVar var@(IntVar s) = do
        st <- get
        case lookup var (intVars st) of
          Nothing -> error $ "undefined variable: " ++ s
          Just rg -> return rg

      -- ポインタ変数を，シンボルテーブルから引っ張ってくる．
      lookupPtr ptr@(PtrVar s) = do
        st <- get
        case lookup ptr (ptrVars st) of
          Nothing -> error $ "undefined pointer variable: " ++ s
          Just pr -> return pr



      defineVar var@(IntVar s) = do
        st <- get
        when (lookup var (intVars st) /= Nothing) (error $ "var already defined" ++ s)
        let regCnt = registers st
        when (regCnt >= localRegisterCount) (error $ "too many variables")
        let regName = Reg regCnt
        put (st { registers = regCnt + 1, intVars = (var, regName):intVars st })
        return $ regName

      definePtr ptr@(PtrVar s) = do
        st <- get
        when (lookup ptr (ptrVars st) /= Nothing) (error $ "var already defined" ++ s)
        let regCnt = ptrRegisters st
        when (regCnt >= localRegisterCount) (error $ "too many ptr vars")
        let regName = PReg regCnt
        put (st { ptrRegisters = regCnt + 1, ptrVars = (ptr, regName):ptrVars st })
        return $ regName

      genLabel = do
        st <- get

        let label = labels st
        put (st { labels = label + 1 })
        return $ Label label

