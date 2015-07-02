{-# LANGUAGE TemplateHaskell #-}

-- このソースコードは、次の記事を信頼して書かれました．
-- http://osecpu.osask.jp/wiki/?page0104
-- http://osecpu.osask.jp/wiki/?page0092


-- 変数のレジスタ割当は，レキシカルスコープのように，ある時点で使用されている変数を管理すれば良い．
-- ラベルの割当は，プログラム中でユニークであるべきなので，コンパイル中全体で管理する．

module ChageComp where

import Data.Word
import Data.List (genericLength, tails)
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
                   , _intRest :: [Reg]
                   , _ptrRest :: [PReg]
                   }

makeLenses ''Frame


emptyFrame :: Frame
emptyFrame = Frame [] [] [Reg 0 .. Reg 0x27] [PReg 0 .. PReg 0x27]


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

apiList = [("api_drawPoint", (Imm 0x0002, [Reg 0x31 .. Reg 0x34])),
           ("api_drawLine",  (Imm 0x0003, [Reg 0x31 .. Reg 0x36])),
           ("api_drawRect",  (Imm 0x0004, [Reg 0x31 .. Reg 0x36])),
           ("api_drawOval",  (Imm 0x0005, [Reg 0x31 .. Reg 0x36])),
           ("api_exit",      (Imm 0x0008, [Reg 0x31 .. Reg 0x31])),
           ("api_sleep",     (Imm 0x0009, [Reg 0x31 .. Reg 0x32])),
           ("api_openWin",   (Imm 0x0010, [Reg 0x31 .. Reg 0x34]))
          ]


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
  when defd (error $ "already defined var: " ++ v)
  reg <- use (frame.intRest) <&> head
  frame.intVars %= ((var, reg) :)
  frame.intRest %= tail
  return $ reg

definePtr ptr@(Var v) = do
  defd <- alreadyDefinedPtr ptr
  when defd (error $ "already defined ptr: " ++ v)
  reg <- use (frame.ptrRest) <&> head
  frame.ptrVars %= ((ptr, reg) :)
  frame.ptrRest %= tail
  return $ reg

-- releaseVar var@(Var v) = do
--  frame.intVars %= filter ((v ==) . fst)
  
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
      compile' irs = concat <$> zipWithM compileIR irs (map concat $ tails $ map IR.varRefs irs)
          
      compileIR :: IR.IR -> [Var] -> State CompileState [Inst]
      compileIR ir livingVars =
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
--         <* mapM_ (\v -> unless (v `elem` livingVars) re) IR.varRefs ir
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
