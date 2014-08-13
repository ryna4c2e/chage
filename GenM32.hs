module GenM32 where

import Data.Word
import Data.Monoid

import System.IO

import qualified Data.ByteString as B
import Data.ByteString.Builder

import Inst


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

instToM32 n = offsetSignature + n


assemble :: Program -> [Word32]
assemble prog = concatMap encode' (instructions prog)
    where
      encode' NOP = [instToM32 0x00]
      encode' (LIMM bit r imm) = [instToM32 0x02,
                                  0xFFFFF788,
                                  toM32 imm,
                                  toM32 r,
                                  toM32 bit]
      encode' (OR   bit r0 r1 r2) = genArith 0x10 bit r0 r1 r2
      encode' (XOR  bit r0 r1 r2) = genArith 0x11 bit r0 r1 r2
      encode' (AND  bit r0 r1 r2) = genArith 0x12 bit r0 r1 r2
      encode' (ADD  bit r0 r1 r2) = genArith 0x14 bit r0 r1 r2
      encode' (SUB  bit r0 r1 r2) = genArith 0x15 bit r0 r1 r2
      encode' (MUL  bit r0 r1 r2) = genArith 0x16 bit r0 r1 r2
      encode' (SHL  bit r0 r1 r2) = genArith 0x18 bit r0 r1 r2
      encode' (SAR  bit r0 r1 r2) = genArith 0x19 bit r0 r1 r2


      genArith code bit r0 r1 r2 = [instToM32 code,
                                    toM32 r1,
                                    toM32 r2,
                                    toM32 r0,                                  
                                    toM32 bit]
                                    

      offset :: Word32
      offset = 0x76000000

toBinary :: [Word32] -> Builder
toBinary w32s = mconcat (map word32BE w32s)

hAssembleOut :: Handle -> Program -> IO ()
hAssembleOut h p = hPutBuilder h $ toBinary $ assemble p
