module ChageParser where


import Control.Applicative hiding (many, (<|>), Const)

import Data.Maybe
import Data.Word

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

import Type
import AST


-- ＿人人人人人人人＿
-- ＞　chageStyle　＜
-- ￣Y^Y^Y^Y^Y^Y￣
chageStyle = javaStyle {
               opStart = oneOf "!&*+./<=>?@\\^|-~{}",
               opLetter = oneOf "!&*+./<=>?@\\^|-~",
               reservedNames = ["if", "while", "int", "else", "var"],
               reservedOpNames = ["+", "-", "*", "/", "<<", ">>", "&", "|", "^", "==", "!=", ">=", "<=", ">", "<", ":=", "$"]
             }


tok = makeTokenParser javaStyle


parseVar :: Parser Var
parseVar = Var <$> identifier tok


parseValue :: Parser (Expr ())
parseValue = (ConstS32Int () <$> (fromInteger <$> integer tok))

parseExpr :: Parser (Expr ())
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
                , comp  "<"  Cmpl
                , comp  "<=" Cmple
                , comp  ">"  Cmpg
                , comp  ">=" Cmpge
                ]
              ]
      infixOperator assoc opName func = Infix (func <$ reservedOp tok opName) assoc
      arith opName constructor = infixOperator AssocLeft opName (Arith () constructor)
      comp  opName constructor = infixOperator AssocNone opName (Comp () constructor)

parseTerm :: Parser (Expr ())
parseTerm = parens tok parseExpr
        <|> parseValue
        <|> (GetVar () <$> parseVar)
        <|> parseLoad
    where
      parseLoad = Load () <$ reservedOp tok "$" <*> parseExpr <*> brackets tok parseExpr



parseChage :: Parser (AST ())
parseChage = parseAST <* spaces <* eof

parseAST :: Parser (AST ())
parseAST = AST <$> many (whiteSpace tok *> parseSentence)

parseSentence = try parseAssign <|>
                try parseIf <|>
                try parseWhile <|>
                try parseDeclare <|>
                try parseStore <|> 
                try parseCall <|>
                try parseBreak <|>
                parseData

parseAssign    = Assign <$>  parseVar
                        <*   reservedOp tok "="
                        <*>  parseExpr
                        <*   semi tok

parseIf        = If     <$   reserved tok "if"
                        <*>  parseExpr
                        <*>  braces tok parseAST
                        <*>  option (AST []) (reserved tok "else" *> braces tok parseAST)

parseWhile     = While   <$  reserved tok "while" <*> parseExpr <*> braces tok parseAST

parseType      = (reserved tok "int" >> return S32Int)
             <|> do reservedOp tok "*"
                    Pointer <$> parseType
parseDeclare   = do reserved tok "var"
                    var <- parseVar
                    reservedOp tok ":"
                    typ <- parseType
                    reservedOp tok "="
                    initial <- parseExpr
                    semi tok
                    return $ Declare var typ initial
                    
                 
parseStore     = Store   <$  reservedOp tok "$"
                         <*> (parseExpr)
                         <*> brackets tok (parseExpr)
                         <*  reservedOp tok "="
                         <*> parseExpr
                         <*  semi tok

parseCall      = Call    <$> identifier tok
                         <*> parens tok (commaSep1 tok parseExpr)
                         <*  semi tok

parseData      = (\pvar words -> Data pvar (map fromInteger words))
               <$  reserved tok "data" 
               <*> parseVar
               <*  reservedOp tok "="
               <*> brackets tok (commaSep1 tok (integer tok))
               <*  semi tok

parseBreak = DebugStop <$ reserved tok "debug" <* semi tok



test = parse parseSentence "<STDIN>"


parseChageFromFile :: FilePath -> IO (AST ())
parseChageFromFile fname = do res <- parseFromFile parseChage fname
                              case res of
                               (Right ast) -> return ast
                               (Left err)  -> error (show err)
                   


                       
