import System.IO
import System.Environment

import Text.Parsec.String
import Numeric

import Inst
import InstParser
import GenM32


import AST
import ChageComp
import ChageParser


readProgram fname = 
    do res <- parseFromFile parseProgram fname
       case res of
         Right res -> return res
         Left err  -> error (show err)


chage fname = do res <- parseFromFile parseChage fname
                 case res of
                   (Right ast@(AST as)) -> let ir = compile ast
                                  in mapM_ print as >> print ir >> return ir
                   (Left err) -> error (show err)

main = do args <- getArgs
          prog <- chage (head args)
          withFile "out.b32" WriteMode
                       (\h -> hAssembleOut h prog)
          

-- main = chage "chag.txt"

