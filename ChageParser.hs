module ChageParser where


import Control.Applicative hiding (many, (<|>), Const)
import Data.Maybe
import Data.Word

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr

import AST


chageStyle = javaStyle {
               opStart = oneOf "!&*+./<=>?@\\^|-~{}",
               opLetter = oneOf "!&*+./<=>?@\\^|-~",
               reservedNames = ["if", "while", "int", "else"],
               reservedOpNames = ["+", "-", "*", "<<", ">>", "&", "|", "^", "==", "!=", ">=", "<=", ">", "<", ":=", "$"]
             }


tok = makeTokenParser javaStyle


parseIntVar :: Parser IntVar
parseIntVar = IntVar <$> identifier tok

parsePtrVar :: Parser PtrVar
parsePtrVar = PtrVar <$> identifier tok


parseValue :: Parser IntValue
parseValue = (Const <$> (fromInteger <$> integer tok)) <|> (GetVar <$> parseIntVar)


parseExpr :: Parser Expr
parseExpr = buildExpressionParser table parseTerm
    where
      table = [
                [ arith "*" Mul ]
              , [ arith "+" Add ]
              , [ arith "&" And ]
              , [ arith "^" Xor ]
              , [ arith "|" Or  ]
              , [
                  comp  "==" Cmpe
                , comp  "!=" Cmpne
                , comp  "<" Cmpl
                , comp  "<=" Cmple
                , comp  ">" Cmpg
                , comp  ">=" Cmpge
                ]
              ]
      infixOperator assoc opName func = Infix (func <$ reservedOp tok opName) assoc
      arith opName constructor = infixOperator AssocLeft opName (Arith constructor)
      comp  opName constructor = infixOperator AssocNone opName (Comp constructor)

parseTerm :: Parser Expr
parseTerm = parens tok parseExpr
        <|> Expr <$> parseValue
        <|> parseLoad
    where
      parseLoad = Load <$ reservedOp tok "$" <*> parsePtrVar <*> brackets tok parseExpr



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
                        <*>  parseExpr
                        <*   semi tok

parseIf        = If     <$   reserved tok "if"
                        <*>  parseExpr
                        <*>  braces tok parseAST
                        <*>  option (AST []) (reserved tok "else" *> braces tok parseAST)

parseWhile     = While   <$  reserved tok "while" <*> parseExpr <*> braces tok parseAST
parseDeclInt   = DeclInt <$  reserved tok "int" <*> parseIntVar <* semi tok
parseDeclPtr   = DeclPtr <$  reserved tok "ptr" <*> parsePtrVar <* semi tok
parsePCopy     = PCopy   <$> parsePtrVar <* reservedOp tok ":=" <*> parsePtrVar <* semi tok
parseStore     = Store   <$  reservedOp tok "$"
                         <*> parsePtrVar
                         <*> brackets tok (parseValue)
                         <*  reservedOp tok "="
                         <*> parseExpr
                         <*  semi tok

parseCall      = Call    <$> identifier tok
                         <*> parens tok (commaSep1 tok parseExpr)
                         <*  semi tok

parseData      = (\pvar words -> Data pvar (map fromInteger words))
               <$  reserved tok "data" 
               <*> parsePtrVar
               <*  reservedOp tok "="
               <*> brackets tok (commaSep1 tok (integer tok))
               <*  semi tok

parseBreak = Break <$ reserved tok "debug" <* semi tok



test = parse parseSentence "<STDIN>"


parseChageFromFile :: FilePath -> IO AST
parseChageFromFile fname = do res <- parseFromFile parseChage fname
                              case res of
                               (Right ast) -> return ast
                               (Left err)  -> error (show err)
                   


                       
