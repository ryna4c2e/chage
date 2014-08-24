module ChageParser where


import Control.Applicative hiding (many, (<|>), Const)
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
               reservedNames = ["if", "while", "int", "else"],
               reservedOpNames = ["+", "-", "*", "==", "!=", ">=", "<=", ">", "<", ":=", "$"]
             }


tok = makeTokenParser javaStyle


parseIntVar :: Parser IntVar
parseIntVar = IntVar <$> identifier tok

parsePtrVar :: Parser PtrVar
parsePtrVar = PtrVar <$> identifier tok


parseValue :: Parser IntValue
parseValue = (Const <$> (fromInteger <$> integer tok)) <|> (GetVar <$> parseIntVar)


parseSimpleExpr :: Parser SimpleExpr
parseSimpleExpr = parseLoad <|>
                   do v1 <- parseValue
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
      parseLoad = do reservedOp tok "$"
                     p <- parsePtrVar
                     ix <- brackets tok parseValue
                     return $ Load p ix

parseChage :: Parser AST
parseChage = do ast <- parseAST 
                spaces
                eof
                return ast

parseAST :: Parser AST
parseAST = AST <$> many (whiteSpace tok >> parseSentence)

parseSentence = try parseAssign <|>
                try parseIf <|>
                try parseWhile <|>
                try parseDeclInt <|> 
                try parseDeclPtr <|> 
                try parsePCopy <|> 
                try parseStore <|> 
                try parseCall <|>
                try parseBreak <|>
                parseData

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

parseWhile = do reserved tok "while"
                cond <- parseSimpleExpr
                body <- braces tok parseAST
                return (While cond body)

parseDeclInt = do reserved tok "int"
                  var <- parseIntVar
                  semi tok
                  return (DeclInt var)

parseDeclPtr = do reserved tok "ptr"
                  var <- parsePtrVar
                  semi tok
                  return (DeclPtr var)


parsePCopy = do p0 <- parsePtrVar
                reservedOp tok ":="
                p1 <- parsePtrVar
                semi tok
                return (PCopy p0 p1)

parseStore = do reservedOp tok "$"
                p <- parsePtrVar
                ix <- brackets tok (parseValue)
                reservedOp tok "="
                expr <- parseSimpleExpr
                semi tok
                return (Store p ix expr)

parseCall = do func <- identifier tok
               args <- parens tok (commaSep1 tok parseSimpleExpr)
               semi tok;

               return (Call func args)

parseData = do reserved tok "data"
               pvar <- parsePtrVar
               reservedOp tok "="
               words <- brackets tok (commaSep1 tok (integer tok))
               semi tok
               return (Data pvar (map fromInteger words))

parseBreak = do reserved tok "debug" >> semi tok >> return Break


test = parseFromFile parseAST "chag.txt"


parseChageFromFile :: FilePath -> IO AST
parseChageFromFile fname = do res <- parseFromFile parseChage fname
                              case res of
                               (Right ast) -> return ast
                               (Left err)  -> error (show err)
                   


                       
