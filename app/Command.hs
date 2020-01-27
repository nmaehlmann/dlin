module Command (Command(..), command) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Text.Parsec.String (Parser)
import Text.Parsec
import Text.Parsec.Char

import AST
import Parser.Lexer
import Parser.Equation

data Command 
    = Load String
    | Define Idt [Int]
    | Evaluate Idt Int
    | SetUpperBound Int

command :: Parser Command
command = try load <|> try define <|> try evaluate <|> try setUpperBound

load :: Parser Command
load = fmap (Load . trimRight) $ symbol ":l" >> (many1 anyChar)

setUpperBound :: Parser Command
setUpperBound = fmap SetUpperBound $ symbol ":b" >> int

int :: Parser Int
int = fmap fromIntegral natural

define :: Parser Command
define = do
    funName <- identifier
    reservedOp "="
    funValues <- brackets $ int `sepBy` comma
    return $ Define funName funValues

evaluate :: Parser Command
evaluate = do
    funName <- identifier
    funArg <- parens int
    return $ Evaluate funName funArg

trimRight :: String -> String
trimRight = dropWhileEnd isSpace