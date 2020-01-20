module Parser.Equation where

import Text.Parsec.String (Parser)
import Text.Parsec

import AST
import Parser.Lexer

identifier :: Parser Idt
identifier = Idt <$> lIdentifier

operation :: Parser Equation
operation = do
    (funName, argName) <- functionCall
    reservedOp "="
    (lhsName, lhsArgName) <- functionCall
    op <- operator'
    (rhsName, rhsArgName) <- functionCall
    if lhsArgName /= argName || rhsArgName /= argName
        then fail "function argument names do not match"
        else return $ OpEq funName $ Operation 
            { operator = op
            , lhs = lhsName
            , rhs = rhsName
            }     

functionCall :: Parser (Idt, Idt)
functionCall = do
    name <- identifier
    arg <- parens identifier
    return (name, arg)

operator' :: Parser Operator
operator' = addOp <|> subOp
    where
        addOp = reservedOp "+" >> return Add
        subOp = reservedOp "-" >> return Sub