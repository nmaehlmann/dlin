module Parser.Lexer where
    
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec
import Data.List

operators :: [String]
operators = 
    [ "+" , "-" , "*" , "/"  
    , "=" , "==", "!=", "<=" 
    , "<" , ">=", ">" , "&&"
    , "||", "[" , "]" , "->"]

operatorSymbols :: [Char]
operatorSymbols = nub $ mconcat operators

names :: [String]
names = 
    [ "ep"
    , "n"
    , "id"
    ]

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
    { Tok.commentStart    = "/*"
    , Tok.commentEnd      = "*/"
    , Tok.commentLine     = "//"
    , Tok.nestedComments  = False
    , Tok.identStart      = alphaNum
    , Tok.identLetter     = alphaNum
    , Tok.opStart         = oneOf operatorSymbols
    , Tok.opLetter        = oneOf operatorSymbols
    , Tok.reservedNames   = names
    , Tok.reservedOpNames = operators
    , Tok.caseSensitive   = True
    }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

lIdentifier :: Parser String
lIdentifier = Tok.identifier lexer

integer :: Parser Integer
integer = Tok.integer lexer

parens :: Parser p -> Parser p
parens = Tok.parens lexer

braces :: Parser p -> Parser p
braces = Tok.braces lexer

brackets :: Parser p -> Parser p
brackets = Tok.brackets lexer

whiteSpace ::  Parser ()
whiteSpace = Tok.whiteSpace lexer

comma ::  Parser String
comma = Tok.comma lexer

commaSep ::  Parser p -> Parser [p]
commaSep = Tok.commaSep lexer

semi ::  Parser String
semi = Tok.semi lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

natural :: Parser Integer
natural = Tok.natural lexer