module GenM32 where

import Data.Word
import Data.Monoid
import Data.List

import System.IO

import qualified Data.ByteString as B
import Data.ByteString.Builder

import Inst


osecpuSignatureM32 :: [Word8]
osecpuSignatureM32 = [0x05, 0xE2, 0x00, 0xCF, 0xEE, 0x7F, 0xF1, 0x88]

intToWord32 :: Int -> Word32
intToWord32 = fromInteger . toInteger


offsetSignature :: Word32
offsetSignature = 0x76000000


class EncodeM32 a where
    toM32 :: a -> Word32

instance EncodeM32 Reg where
    toM32 (Reg r) = fromInteger (toInteger r) + offsetSignature

instance EncodeM32 BitSpec where
    toM32 (BitSpec r) = fromInteger (toInteger r) + offsetSignature

instance EncodeM32 Imm where
    toM32 (Imm r) = fromInteger (toInteger r)

instance EncodeM32 PReg where
    toM32 (PReg p) = fromInteger (toInteger p) + offsetSignature

instance EncodeM32 Label where
    toM32 (Label l) = fromInteger (toInteger l) + offsetSignature

instance EncodeM32 LabelOpt where
    toM32 (LabelOpt o) = fromInteger (toInteger o) + offsetSignature


instToM32 n = offsetSignature + n
sInt32Type = instToM32 0x06


assemble :: Program -> [Word32]
assemble prog = concatMap encode' (instructions prog)
    where
      encode' NOP = [instToM32 0x00]
      encode' (LIMM bit r imm) = [instToM32 0x02,
                                  0xFFFFF788,
                                  toM32 imm,
                                  toM32 r,
                                  toM32 bit]
      encode' (LB   opt label) = [instToM32 0x01,
                                  toM32 label,
                                  toM32 opt]
      encode' (PLIMM p label) = [instToM32 0x03,
                                 toM32 label,
                                 toM32 p]
      encode' (PCP   p0 p1)   = [instToM32 0x1e,
                                 toM32 p1,
                                 toM32 p0]
      encode' (CND  reg) = [instToM32 0x04, toM32 reg]

      encode' (LMEM0 bit r p) = [instToM32 0x08,
                                 toM32 p,
                                 sInt32Type,
                                 instToM32 0,
                                 toM32 r,
                                 toM32 bit]
-- 76000009	r	bit	p	typ	76000000
      encode' (SMEM0 bit r p) = [instToM32 0x09,
                                 toM32 r,
                                 toM32 bit,
                                 toM32 p,
                                 sInt32Type,
                                 instToM32 0]

-- 7600000E	p1	typ	r	bit	p0	
      encode' (PADD bit p0 p1 r) = [instToM32 0x0e,
                                    toM32 p1,
                                    sInt32Type,
                                    toM32 r,
                                    toM32 bit,
                                    toM32 p0]

      encode' (OR   bit r0 r1 r2) = genArith 0x10 bit r0 r1 r2
      encode' (XOR  bit r0 r1 r2) = genArith 0x11 bit r0 r1 r2
      encode' (AND  bit r0 r1 r2) = genArith 0x12 bit r0 r1 r2
      encode' (SBX  bit r0 r1 r2) = genArith 0x13 bit r0 r1 r2
      encode' (ADD  bit r0 r1 r2) = genArith 0x14 bit r0 r1 r2
      encode' (SUB  bit r0 r1 r2) = genArith 0x15 bit r0 r1 r2
      encode' (MUL  bit r0 r1 r2) = genArith 0x16 bit r0 r1 r2
      encode' (SHL  bit r0 r1 r2) = genArith 0x18 bit r0 r1 r2
      encode' (SAR  bit r0 r1 r2) = genArith 0x19 bit r0 r1 r2
      encode' (DIV  bit r0 r1 r2) = genArith 0x1A bit r0 r1 r2
      encode' (MOD  bit r0 r1 r2) = genArith 0x1B bit r0 r1 r2

      encode' (CMPE  bit0 bit1 r0 r1 r2) = genComp 0x20 bit0 bit1 r0 r1 r2
      encode' (CMPNE bit0 bit1 r0 r1 r2) = genComp 0x21 bit0 bit1 r0 r1 r2
      encode' (CMPL  bit0 bit1 r0 r1 r2) = genComp 0x22 bit0 bit1 r0 r1 r2
      encode' (CMPGE bit0 bit1 r0 r1 r2) = genComp 0x23 bit0 bit1 r0 r1 r2
      encode' (CMPLE bit0 bit1 r0 r1 r2) = genComp 0x24 bit0 bit1 r0 r1 r2
      encode' (CMPG  bit0 bit1 r0 r1 r2) = genComp 0x25 bit0 bit1 r0 r1 r2
      encode' (TSTZ  bit0 bit1 r0 r1 r2) = genComp 0x26 bit0 bit1 r0 r1 r2
      encode' (TSTNZ bit0 bit1 r0 r1 r2) = genComp 0x27 bit0 bit1 r0 r1 r2


      encode' (DATA ws) = [instToM32 0x2e,
                           sInt32Type,
                           instToM32 (genericLength ws)
                           ] ++ ws

      genArith code bit r0 r1 r2 = [instToM32 code,
                                    toM32 r1,
                                    toM32 r2,
                                    toM32 r0,                                  
                                    toM32 bit]
                                    
      genComp code bit0 bit1 r0 r1 r2 = [instToM32 code,
                                         toM32 r1,
                                         toM32 r2,
                                         toM32 bit1,
                                         toM32 r0,                                  
                                         toM32 bit0]


      offset :: Word32
      offset = 0x76000000

toBinary :: [Word32] -> Builder
toBinary w32s = mconcat (map word8 osecpuSignatureM32 ++ map word32BE w32s)

hAssembleOut :: Handle -> Program -> IO ()
hAssembleOut h p = hPutBuilder h $ toBinary $ assemble p
