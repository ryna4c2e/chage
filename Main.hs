module Main where


import Control.Monad
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
import Typing
import IR


-- 参考: http://d.hatena.ne.jp/kazu-yamamoto/20090107/1231318138
-- https://hackage.haskell.org/package/base-4.7.0.1/docs/System-Console-GetOpt.html
data Flag
  = Language (FilePath -> IO Program)
  | Output FilePath

data ChageOptions = ChageOptions
    { optLanguage :: FilePath -> IO Program
    , optOutput   :: FilePath
    }


defaultOptions = ChageOptions
    { optLanguage = chage
    , optOutput   = "out.b32"
    }
                   
options :: [OptDescr (ChageOptions -> ChageOptions)]
options =
  [ Option "x"  ["language"] (OptArg langp "chage|inst") "language"
  , Option "o"  ["output"]   (OptArg outp "FILE")      "output file name"
  ]


-- langp, outpは、オプションの文字列 -> 元々あったオプション -> 付け加えたオプション, という関数。
langp :: Maybe String -> ChageOptions -> ChageOptions
langp input options = options { optLanguage = selectLanguage }
    where
      selectLanguage = case input of
                         Just "chage"  -> chage
                         Just "inst"   -> inst
                         Just langName -> error $ "unknown language: " ++ langName
                         _             -> chage


outp :: Maybe String -> ChageOptions -> ChageOptions
outp fileName options = options { optOutput = fromMaybe "out.b32" fileName }


-- instは、osecpu-vmの生の命令列をアセンブルする。                        
inst :: FilePath -> IO Program
inst = parseInstFromFile

-- chageは、ちょっと高級な言語のコンパイルを担当する。
chage :: FilePath -> IO Program
chage fname = do ast <- parseChageFromFile fname
                 return $ compile (normalize (typing ast))


chageOptions :: [String] -> IO (ChageOptions, [String])
chageOptions args = case getOpt Permute options args of
                     (o, n, []   ) -> return (foldr ($) defaultOptions o, n)
                     (_, _, errs ) -> ioError (userError (concat errs ++ usageInfo header options))
    where
      header = "usage: chage [OPTIONS...] file"



main = do args <- getArgs
          (options, args) <- chageOptions args

          when (length args == 0) $ fail "please specify a input file name"

          let lang = optLanguage options
          let outFname = optOutput options
          let inputFname = head args
                           
          prog <- lang inputFname
                  
          withFile outFname WriteMode (\h -> hAssembleOut h prog)
          

-- main = chage "chag.txt"

