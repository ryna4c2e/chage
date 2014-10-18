module Inst where

import Numeric
import Data.Word

newtype Reg     = Reg Word32 deriving (Eq)
newtype PReg    = PReg Word32 deriving (Eq)
newtype BitSpec = BitSpec Word32 deriving (Eq)
newtype Imm     = Imm Word32 deriving (Eq)
newtype Label   = Label Word32 deriving (Eq)
newtype LabelOpt = LabelOpt Word32 deriving (Eq)

data Inst = NOP
          | LIMM BitSpec Reg Imm

          | LB    LabelOpt Label

          | PLIMM PReg Label
          | PCP   PReg PReg

            -- Condition
          | CND   Reg

          | LMEM0 BitSpec Reg PReg
          | SMEM0 BitSpec Reg PReg

          | PADD BitSpec PReg PReg Reg

            -- Arithmetic & Logical Operators
          | OR  BitSpec Reg Reg Reg
          | XOR BitSpec Reg Reg Reg
          | AND BitSpec Reg Reg Reg
          | SBX BitSpec Reg Reg Reg
          | ADD BitSpec Reg Reg Reg
          | SUB BitSpec Reg Reg Reg
          | MUL BitSpec Reg Reg Reg
          | SHL BitSpec Reg Reg Reg
          | SAR BitSpec Reg Reg Reg
          | DIV BitSpec Reg Reg Reg
          | MOD BitSpec Reg Reg Reg

            -- Compare Operators
          | CMPE  BitSpec BitSpec Reg Reg Reg
          | CMPNE BitSpec BitSpec Reg Reg Reg
          | CMPL  BitSpec BitSpec Reg Reg Reg
          | CMPGE BitSpec BitSpec Reg Reg Reg
          | CMPLE BitSpec BitSpec Reg Reg Reg
          | CMPG  BitSpec BitSpec Reg Reg Reg
          | TSTZ  BitSpec BitSpec Reg Reg Reg
          | TSTNZ BitSpec BitSpec Reg Reg Reg

          | DATA  [Word32]

          | BREAK

            deriving (Show, Eq)

data Program = Program { instructions :: [Inst] } deriving (Eq)

instance Show Reg where
    showsPrec d (Reg r) = showString "R" . showHex r

instance Show PReg where
    showsPrec d (PReg r) = showString "P" . showHex r

instance Show BitSpec where
    showsPrec d (BitSpec b) = showInt b

instance Show Imm where
    showsPrec d (Imm b) = showInt b

instance Show Label where
    showsPrec d (Label b) = showInt b

instance Show LabelOpt where
    showsPrec d (LabelOpt b) = showInt b

instance Show Program where
    showsPrec d (Program insts) = foldl (.) id $ map (\i -> showsPrec 1 i . showString "\n") insts
