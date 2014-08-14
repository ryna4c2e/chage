module ChageParser where


import Control.Applicative hiding (many, (<|>))
import Data.Maybe
import Data.Word

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

import AST


chageStyle = javaStyle {
               opStart = oneOf "!&*+./<=>?@\\^|-~{}",
               opLetter = oneOf "!&*+./<=>?@\\^|-~",
               reservedNames = ["if", "while", "data"],
               reservedOpNames = ["+", "-", "*", "==", "!=", ">=", "<=", ">", "<"]
             }


tok = makeTokenParser javaStyle


parseIntVar :: Parser IntVar
parseIntVar = IntVar <$> identifier tok


parseValue :: Parser IntValue
parseValue = (Imm <$> (fromInteger <$> integer tok)) <|> (GetVar <$> parseIntVar)


parseSimpleExpr :: Parser SimpleExpr
parseSimpleExpr = do v1 <- parseValue
                     (do op <- parseOp
                         v2 <- parseValue
                         return $ op v1 v2) <|> return (Expr v1)
    where
      parseOp = do str <- operator tok
                   case lookup str db of
                     Nothing -> unexpected str
                     Just op -> return op

          where
            db = [("+", Add),
                  ("*", Mul),
                  ("==", Cmpe),
                  ("!=", Cmpne),
                  (">", Cmpg),
                  ("<", Cmpl),
                  (">=", Cmpge),
                  ("<=", Cmple)
                  ]


parseAST :: Parser AST
parseAST = many parseSentence

parseSentence = parseAssign <|> parseIf <|> parseWhile <|> parseDecl

parseAssign = do var <- parseIntVar
                 reservedOp tok "="
                 val <- parseSimpleExpr
                 semi tok
                 return $ Assign var val                        

parseIf = do reserved tok "if"
             cond <- parseSimpleExpr
             csqt <- braces tok parseAST
             reserved tok "else"
             altn <- braces tok parseAST
             return (If cond csqt altn)

test = parseFromFile parseAssign "chag.txt"