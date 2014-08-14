module InstParser where

import Control.Applicative hiding (many)
import Data.Maybe

import Data.Word


import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

import Inst




blanks = many (oneOf " \t")

parseHex :: Parser Word32
parseHex = read <$> ("0x" ++) <$> many1 (oneOf (['0'..'9'] ++ "abcdef" ++ "ABCDEF"))

parseDecimal :: Parser Word32
parseDecimal = read <$> many1 (oneOf ['0'..'9'])


parseProgram :: Parser Program
parseProgram = do
  maybeList <- sepBy1 parseLine newline
  blanks
  eof
  return $ Program $ catMaybes maybeList


parseLine :: Parser (Maybe Inst)
parseLine = do blanks
               inst <- option Nothing (Just <$> parseInst)
               blanks
               return inst

parseInst :: Parser Inst
parseInst = do
  blanks
  name <- many1 lower
  case lookup name db of
    Nothing -> fail $ "unknown inst: " ++ name
    Just p  -> p

    where
      db = [("nop",   return NOP)
           ,("limm",  limm)
           ,("lb",    lb)

           ,("plimm", plimm)
           ,("pcp",   pcp)

           ,("lmem",  buildMem LMEM0)
           ,("smem",  buildMem SMEM0)

           ,("padd",  padd)

           ,("cnd",   cnd)

           ,("or",    buildArith OR)
           ,("xor",   buildArith XOR)
           ,("and",   buildArith AND)
           ,("sbx",   buildArith SBX)
           ,("add",   buildArith ADD)
           ,("sub",   buildArith SUB)
           ,("mul",   buildArith MUL)
           ,("shl",   buildArith SHL)
           ,("sar",   buildArith SAR)
           ,("div",   buildArith DIV)
           ,("mod",   buildArith MOD)

           ,("cmpe",  buildComp CMPE)
           ,("cmpne", buildComp CMPNE)
           ,("cmpl",  buildComp CMPL)
           ,("cmpge", buildComp CMPGE)
           ,("cmple", buildComp CMPLE)
           ,("cmpg",  buildComp CMPG)
           ,("tstz",  buildComp TSTZ)
           ,("tstnz", buildComp TSTNZ)
            
           ,("data", parseData)
           ]

      limm  = LIMM  <$> parseBitSpec <*> parseReg <*> parseImm
      lb    = LB    <$> parseLabelOpt <*> parseLabel
      plimm = PLIMM <$> parsePReg <*> parseLabel

      pcp   = PCP   <$> parsePReg <*> parsePReg

      cnd   = CND   <$> parseReg

      buildArith op = do bit <- parseBitSpec
                         r1  <- parseReg <?> "first register"
                         r2  <- parseReg <?> "second register"
                         r3  <- parseReg <?> "third register"
                         return $ op bit r1 r2 r3

      buildComp op  = do bit0 <- parseBitSpec
                         bit1 <- parseBitSpec
                         r0  <- parseReg <?> "first register"
                         r1  <- parseReg <?> "second register"
                         r2  <- parseReg <?> "third register"
                         return $ op bit0 bit1 r0 r1 r2

      buildMem op = do bit <- parseBitSpec
                       r   <- parseReg
                       p <- parsePReg
                       return $ op bit r p

      padd = PADD <$> parseBitSpec <*> parsePReg <*> parsePReg <*> parseReg

      parseData = do spaces
                     char '['
                     storedData <- sepBy1 (spaces >> parseDecimal) (char ',')
                     spaces
                     char ']'
                     return $ DATA storedData

-- 76000008	p	typ	76000000	r	bit		LMEM0(bit, r, p);

parseBitSpec :: Parser BitSpec
parseBitSpec = do {
                 blanks;
                 d <- parseDecimal;
                 return $ BitSpec d;
               } <?> "bit spec"


parseReg :: Parser Reg
parseReg = do
  blanks
  oneOf "rR"
  n <- parseHex
  return $ Reg n

parsePReg :: Parser PReg
parsePReg = do
  blanks
  oneOf "pP"
  n <- parseHex
  return $ PReg n




parseImm :: Parser Imm
parseImm = do {
             blanks;
             d <- parseDecimal;
             return $ Imm d;
           } <?> "immediate value"


parseLabel :: Parser Label
parseLabel = do {
               blanks;
               d <- parseDecimal;
               return $ Label d
             } <?> "label number"


parseLabelOpt :: Parser LabelOpt
parseLabelOpt = do {
             blanks;
             d <- read . (:[]) <$> oneOf "0123";
             return $ LabelOpt d;
           } <?> "label option"



test = do
  res <- parseFromFile parseProgram "input.txt"
  case res of
    Right res -> mapM_ print (instructions res)
    Left err  -> print err
