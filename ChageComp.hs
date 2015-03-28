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
import Control.Applicative
    
import Inst
import AST 
import Type
import Typing
import qualified IR
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


{-
-- 全て艦これが悪い
normalize :: AST Type -> AST Type
normalize ast = evalState (normalize' ast) 0
    where
      genSym :: State Int Var
      genSym = do st <- get
                  modify (1 +)
                  return $ Var $ "G" ++ show st
                         
      normalize' :: AST Type -> State Int (AST Type)
      normalize' (AST stmts) = do sss <- forM stmts normalizeSentence
                                  return $ AST (concat sss)

      normalizeExpr :: Expr Type -> Var -> State Int [Sentence Type]
      normalizeExpr expr var =
          case expr of
            Arith t op e1 e2 ->
                do (n1, v1) <- assignNewVar e1
                   (n2, v2) <- assignNewVar e2
                   return $ n1 ++ n2 ++ [Declare var t $ Arith t op v1 v2]
            Comp t op e1 e2 ->
                do (n1, v1) <- assignNewVar e1
                   (n2, v2) <- assignNewVar e2
                   return $ n1 ++ n2 ++ [Declare var t $ Comp t op v1 v2]
            ConstS32Int t v -> return [Declare var S32Int $ ConstS32Int t v]
            GetVar t v ->
                do s <- genSym
                   return [Declare var t $ GetVar t v]
            Load t ptr idx ->
                do (n1, v1) <- assignNewVar ptr
                   (n2, v2) <- assignNewVar idx
                   return $ n1 ++ n2 ++ [Declare var t $ Load t v1 v2]


      assignNewVar expr = do s <- genSym
                             n <- normalizeExpr expr s
                             return (n, GetVar (typeOf expr) s)

      normalizeSentence :: Sentence Type -> State Int [Sentence Type]
      normalizeSentence sentence =
          case sentence of
            Assign var expr -> normalizeExpr expr var
            If cnd csq alt  ->
                do s <- genSym
                   (n1, v1) <- assignNewVar cnd
                   a1 <- normalize' csq
                   a2 <- normalize' alt
                   return $ n1 ++ [If v1 a1 a2]
            While cnd body ->
                do s <- genSym
                   (n, v) <- assignNewVar cnd
                   a <- normalize' body
                   return $ n ++ [While v a]
            Declare var t expr -> normalizeExpr expr var
            Store dat idx val ->
                do (n1, v1) <- assignNewVar dat
                   (n2, v2) <- assignNewVar idx
                   (n3, v3) <- assignNewVar val
                   return $ n1 ++ n2 ++ n3 ++ [Store v1 v2 v3]
            Call name exprs ->
                do ss <- mapM (\_ -> genSym) exprs
                   ns <- sequence $ zipWith normalizeExpr exprs ss
                   return $ concat ns ++ [Call name (map (GetVar S32Int) ss)]
            Data var ws -> return [Data var ws]
            DebugStop -> return [DebugStop]

-}                         


{-
compile :: AST -> Program
compile ast = Program $ evalState (compAST ast) (CS 0 emptyFrame)
    where
      compAST :: AST Type -> State CompileState [Inst]
      compAST (AST ss) = concat <$> mapM compSentence ss

      compSentence :: Sentence Type -> State CompileState [Inst]
      compSentence (Declare var S32Int (GetVar _ val)) =
          do defineVar var
             r <- lookupVar var
             v <- lookupVar val
             return [OR spec32 var val val]

      compSentence (Declare var (Pointer _) (GetVar _ val)) =
          do definePtr var
             p <- lookupPtr var
             v <- lookupPtr val
             return [PCP p v]

      compSentence (Store (GetVar (Pointer S32Int) ptr) (GetVar _ ix) (GetVar S32Int val)) =
          do p <- lookupPtr ptr
             i <- lookupVar ix
             v <- lookupVar val
             return [PADD spec32 p3e p i, SMEM0 spec32 v p3e]

      compSentence (Data ptrvar ws) =
          do definePtr ptrvar
             p <- lookupPtr ptrvar
             lbl <- genLabel
             return $ [PLIMM p lbl, LB readWrite lbl, DATA ws]

      compSentence (Assign var (GetVar typ val)) =
          case typ of
            S32Int    -> do r <- lookupVar var
                            v <- lookupVar val
                            [OR spec32 var val val]                            
            Pointer _ -> do p <- lookupPtr var
                            v <- lookupPtr val
                            [PCP var val]

      compSentence (If (GetVar _ cond) cnsq altn) =
          do c1 <- extendScope (compAST cnsq)
             c2 <- extendScope (compAST altn)

             ifLabel <- genLabel
             endLabel  <- genLabel

             return $ [LIMM spec32 tmp2 (Imm 1),
                       XOR spec32 tmp1 cond tmp2,
                       CND tmp1] ++ 
                        c1 ++ [PLIMM p3f endLabel, LB jumpOnly ifLabel] ++
                        c2 ++ [LB jumpOnly endLabel]

      compSentence (While cond body) =
          do c <- extendScope (compAST body)

             startLabel <- genLabel
             endLabel <- genLabel

             return $ [LB jumpOnly startLabel] ++ c1 ++
                        [LIMM spec32 tmp2 (Imm 1), XOR spec32 tmp1 tmp1 tmp2,
                         CND tmp1, PLIMM p3f endLabel] ++
                          c ++ [PLIMM p3f startLabel, LB jumpOnly endLabel]

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


-}
apiList = [("api_drawPoint", (Imm 0x0002, [Reg 0x31, Reg 0x32, Reg 0x33, Reg 0x34])),
           ("api_sleep",     (Imm 0x0009, [Reg 0x31, Reg 0x32])),
           ("api_openWin",   (Imm 0x0010, [Reg 0x31, Reg 0x32, Reg 0x33, Reg 0x34])),
           ("api_fillOval",  (Imm 0x0005, [Reg 0x31, Reg 0x32, Reg 0x33, Reg 0x34, Reg 0x35, Reg 0x36]))]

{-
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

-}

-- スコープのネストした中でモナドを走らせる．要は，いったん戻してるだけ．
extendScope :: State CompileState a -> State CompileState a
extendScope m = do
  fr <- use frame
  ret <- m
  frame .= fr
  return ret

-- 整数の変数を，シンボルテーブルから引っ張ってくる．
lookupVar :: Var -> State CompileState Reg
lookupVar var@(Var s) = do
  vars <- use (frame.intVars)
  case lookup var vars of
    Nothing -> error $ "undefined variable: " ++ s
    Just rg -> return rg

-- ポインタ変数を，シンボルテーブルから引っ張ってくる．
lookupPtr :: Var -> State CompileState PReg
lookupPtr ptr@(Var s) = do
  vars <- use (frame.ptrVars)
  case lookup ptr vars of
    Nothing -> error $ "undefined pointer variable: " ++ s
    Just pr -> return pr

alreadyDefinedVar :: Var -> State CompileState Bool
alreadyDefinedVar v = do
  vars <- use (frame.intVars)
  return $ case lookup v vars of
    Nothing -> False
    Just _  -> True

alreadyDefinedPtr v = do
  vars <- use (frame.ptrVars)
  return $ case lookup v vars of
    Nothing -> False
    Just _  -> True
               
defineVar var@(Var v) = do
  defd <- alreadyDefinedVar var
  when defd (error $ "already defined" ++ v)
  reg <- use (frame.intVars) <&> genericLength <&> Reg
  frame.intVars %= ((var, reg) :)
  return $ reg

definePtr ptr@(Var v) = do
  defd <- alreadyDefinedPtr ptr
  when defd (error $ "already defined" ++ v)
  reg <- use (frame.ptrVars) <&> genericLength <&> PReg
  frame.ptrVars %= ((ptr, reg) :)
  return $ reg

genLabel :: State CompileState Label
genLabel = do
  lb <- use labels
  labels += 1
  return $ Label lb

arith And = AND
arith Xor = XOR
arith Or  = OR
arith Add = ADD
arith Mul = MUL

comp  Cmpe = CMPE
comp  Cmpne = CMPNE
comp  Cmpl = CMPL
comp  Cmple = CMPLE
comp  Cmpg = CMPG
comp  Cmpge = CMPGE

compile :: [IR.IR] -> Program
compile is = Program $ evalState (compile' is) (CS 0 emptyFrame)
    where
      compile' :: [IR.IR] -> State CompileState [Inst]
      compile' irs = concat <$> mapM compileIR irs
          
      compileIR :: IR.IR -> State CompileState [Inst]
      compileIR ir =
          case ir of
            IR.If var csq alt ->
                do cond <- lookupVar var
                   c1 <- extendScope (compile' csq)
                   c2 <- extendScope (compile' alt)
                  
                   ifLabel <- genLabel
                   endLabel <- genLabel

                   return $ [LIMM spec32 tmp2 (Imm 1),
                             XOR  spec32 tmp1 cond tmp2,
                             CND tmp1, PLIMM p3f ifLabel] ++
                              c1 ++ [PLIMM p3f endLabel, LB jumpOnly ifLabel] ++
                              c2 ++ [LB jumpOnly endLabel]
            IR.While var cond body ->
                do c1 <- compile' cond
                   cv <- lookupVar var
                   c2 <- extendScope (compile' body)
                   startLabel <- genLabel
                   endLabel <- genLabel
                   return $ [LB jumpOnly startLabel] ++ c1 ++
                            [LIMM spec32 tmp2 (Imm 1), XOR spec32 tmp1 cv tmp2,
                             CND tmp1, PLIMM p3f endLabel] ++
                             c2 ++ [PLIMM p3f startLabel, LB jumpOnly endLabel]

            IR.Declare var S32Int val ->
                do defineVar var
                   r <- lookupVar var
                   v <- lookupVar val
                   return [OR spec32 r v v]
            IR.Declare var (Pointer _) val ->
                do definePtr var
                   p <- lookupPtr var
                   v <- lookupPtr val
                   return [PCP p v]
            IR.Arith op var e1 e2 ->
                do defineVar var
                   r  <- lookupVar var
                   v1 <- lookupVar e1
                   v2 <- lookupVar e2
                   return [arith op spec32 r v1 v2]

            IR.Comp op var e1 e2 ->
                do defineVar var
                   r  <- lookupVar var
                   v1 <- lookupVar e1
                   v2 <- lookupVar e2
                   return [comp op spec32 spec32  r v1 v2]
            IR.ConstS32Int var int ->
                do defineVar var
                   r <- lookupVar var
                   return [LIMM spec32 r (Imm (fromInteger (toInteger int)))]
            IR.Assign var S32Int val ->
                do r <- lookupVar var
                   v <- lookupVar val
                   return [OR spec32 r v v]
            IR.Assign var (Pointer _) val ->
                do p <- lookupPtr var
                   v <- lookupPtr val
                   return [PCP p v]

            IR.Load var ptr idx ->
                do defineVar var
                   v <- lookupVar var
                   p <- lookupPtr ptr
                   i <- lookupVar idx
                   return [PADD spec32 p3e p i, LMEM0 spec32 v p3e]
            IR.Store ptr idx val ->
                do p <- lookupPtr ptr
                   i <- lookupVar idx
                   v <- lookupVar val
                   return [PADD spec32 p3e p i, SMEM0 spec32 v p3e]
            IR.Call name vars ->
                case lookup name apiList of
                  Nothing -> error $ "function not found: " ++ name
                  Just (inst, regs) -> do  
                    args <- mapM lookupVar vars
                    let move = zipWith (\d s -> OR spec32 d s s) regs args
                    lb <- genLabel
                    return $ move ++ [LIMM spec32 (Reg 0x30) inst,
                                      PLIMM p30 lb,
                                      PCP p3f p2f,
                                      LB readWrite lb]
            IR.Data name dat ->
                do definePtr name
                   p <- lookupPtr name
                   lb <- genLabel
                   return $ [PLIMM p lb, LB readWrite lb, DATA dat]
            IR.DebugStop -> return [BREAK]
test1 = AST [
         Declare (Var "x") S32Int (Arith () Add
                                   (Arith () Xor (ConstS32Int () 3) (ConstS32Int () 4))
                                   (ConstS32Int () 5))]
test2 = AST [
         Data    (Var "d") [1, 2, 3],
         Declare (Var "y") S32Int (Load () (GetVar () (Var "d")) (ConstS32Int () 1)),
         Store (GetVar () (Var "d")) (ConstS32Int () 3) (ConstS32Int () 4)
        ]

test3 = AST [
         Data (Var "d") [1, 2, 3],
         Declare (Var "y") (Pointer S32Int) (GetVar () (Var "d"))
        ]
test4 = AST [
         Declare (Var "i") S32Int (ConstS32Int () 0),
         While (GetVar () (Var "i")) (AST [])
        ]
test5 = AST [
         If (Comp () Cmpe (ConstS32Int () 3) (ConstS32Int () 4))
            (AST [])
            (AST [])
        ]

process = compile . IR.normalize . typing
