import System.IO

import Text.Parsec.String
import Numeric

import Inst
import Parser
import GenM32



readProgram fname = 
    do res <- parseFromFile parseProgram fname
       case res of
         Right res -> return res
         Left err  -> error (show err)



fillDigits n s = replicate (n - length s) '0' ++ s
printHex = mapM_ (putStrLn . fillDigits 8 . flip showHex "")


main = do prog <- readProgram "input.txt"
          withFile "out.b32" WriteMode
                       (\h -> hAssembleOut h prog)
          

