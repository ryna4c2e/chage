module Inst where

newtype Reg     = Reg Int deriving (Show, Eq)
newtype PReg    = PReg Int deriving (Show, Eq)
newtype BitSpec = BitSpec Int deriving (Show, Eq)
newtype Imm     = Imm Int deriving (Show, Eq)
newtype Label   = Label Int deriving (Show, Eq)
newtype LabelOpt = LabelOpt Int deriving (Show, Eq)

data Inst = NOP
          | LIMM BitSpec Reg Imm

          | LB    LabelOpt Label
          | PLIMM PReg Label

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

            deriving (Show, Eq)

data Program = Program { instructions :: [Inst] } deriving (Show, Eq)

