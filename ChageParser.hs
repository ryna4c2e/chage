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
      parseLoad = Load <$ reservedOp tok "$" <*> parsePtrVar <*> brackets tok parseValue

      parseOp = do str <- operator tok
                   case lookup str db of
                     Nothing -> unexpected str
                     Just op -> return op

          where
            db = [
                   ("+", Add)
                 , ("*", Mul)
                 , ("==", Cmpe)
                 , ("!=", Cmpne)
                 , (">", Cmpg)
                 , ("<", Cmpl)
                 , (">=", Cmpge)
                 , ("<=", Cmple)
                 ]

parseChage :: Parser AST
parseChage = parseAST <* spaces <* eof

parseAST :: Parser AST
parseAST = AST <$> many (whiteSpace tok *> parseSentence)

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

parseAssign    = Assign <$>  parseIntVar
                        <*   reservedOp tok "="
                        <*>  parseSimpleExpr
                        <*   semi tok

parseIf        = If     <$   reserved tok "if"
                        <*>  parseSimpleExpr
                        <*>  braces tok parseAST 
                        <*   reserved tok "else" 
                        <*>  braces tok parseAST

parseWhile     = While   <$  reserved tok "while" <*> parseSimpleExpr <*> braces tok parseAST
parseDeclInt   = DeclInt <$  reserved tok "int" <*> parseIntVar <* semi tok
parseDeclPtr   = DeclPtr <$  reserved tok "ptr" <*> parsePtrVar <* semi tok
parsePCopy     = PCopy   <$> parsePtrVar <* reservedOp tok ":=" <*> parsePtrVar <* semi tok
parseStore     = Store   <$  reservedOp tok "$"
                         <*> parsePtrVar
                         <*> brackets tok (parseValue)
                         <*  reservedOp tok "="
                         <*> parseSimpleExpr
                         <*  semi tok

parseCall      = Call    <$> identifier tok
                         <*> parens tok (commaSep1 tok parseSimpleExpr)
                         <*  semi tok

parseData      = (\pvar words -> Data pvar (map fromInteger words))
               <$  reserved tok "data" 
               <*> parsePtrVar
               <*  reservedOp tok "="
               <*> brackets tok (commaSep1 tok (integer tok))
               <*  semi tok

parseBreak = Break <$ reserved tok "debug" <* semi tok



test = parseFromFile parseAST "test.chag"


parseChageFromFile :: FilePath -> IO AST
parseChageFromFile fname = do res <- parseFromFile parseChage fname
                              case res of
                               (Right ast) -> return ast
                               (Left err)  -> error (show err)
                   


                       
