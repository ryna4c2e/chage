module Main where


import System.IO
import System.Environment
import System.Console.GetOpt

import Text.Parsec.String
import Numeric
import Data.Maybe (fromMaybe)

import Inst
import InstParser
import GenM32

import AST
import ChageComp
import ChageParser

data Flag
  = Language (FilePath -> IO Program)
  | Output FilePath

options :: [OptDescr Flag]
options =
  [ Option "x"  ["language"] (ReqArg langp "chage|inst") "language"
  , Option "o"  ["output"]   (ReqArg Output "FILE")        "output file name"
  ]


langp :: String -> Flag
langp input = case input of
               "chage"  -> Language chage
               "inst"   -> Language inst
               langName -> error $ "unknown language: " ++ langName


inst :: FilePath -> IO Program
inst = parseInstFromFile

chage :: FilePath -> IO Program
chage fname = do ast <- parseChageFromFile fname
                 return $ compile ast


chageOptions :: [String] -> IO ([Flag], [String])
chageOptions argv = case getOpt Permute options argv of
                     (o, n, []   ) -> return (o, n)
                     (_, _, errs ) -> ioError (userError (concat errs ++ usageInfo header options))
    where
      header = "usage: chage [OPTIONS...] file"


-- TODO: オプション解釈をちゃんと作る

main = do [f] <- getArgs
          prog <- chage f
          withFile "out.b32" WriteMode
                       (\h -> hAssembleOut h prog)
          

-- main = chage "chag.txt"

