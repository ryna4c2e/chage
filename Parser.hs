{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser where

import Control.Applicative hiding (many)
import Data.Maybe


import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

import Inst




blanks = many (oneOf " \t")

parseDecimal :: Parser Int
parseDecimal = read <$> many1 (oneOf ['0'..'9'])

parseProgram :: Parser Program
parseProgram = do
  maybeList <- sepBy1 parseLine newline
  blanks
  eof
  return $ Program $ catMaybes maybeList


-- seqSepBy1 (x:[]) by = do n <- x
--                          return x
-- seqSepBy1 (x:xs) by = do n <- x
--                          by
--                          ns <- seqSepBy1 xs
--                          return (n:ns)


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
      db = [("nop", return NOP)
           ,("limm", limm)
           ,("or" , buildArith OR)
           ,("xor", buildArith XOR)
           ,("and", buildArith AND)
--           ,("sbx", buildArith ADD),
           ,("add", buildArith ADD)]

      limm = do bit <- parseBitSpec
                r   <- parseReg
                imm <- parseImm
                return $ LIMM bit r imm

      buildArith op = do bit <- parseBitSpec
                         r1  <- parseReg <?> "first register"
                         r2  <- parseReg <?> "second register"
                         r3  <- parseReg <?> "third register"
                         return $ op bit r1 r2 r3

  

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
  n <- parseDecimal
  return $ Reg n


parseImm :: Parser Imm
parseImm = do {
             blanks;
             d <- parseDecimal;
             return $ Imm d;
           } <?> "immediate value"


