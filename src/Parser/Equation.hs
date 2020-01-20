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
recursion :: Parser Equation
recursion = do
    (funName, argName) <- functionCall
    reservedOp "="
    outerFunName <- identifier
    (innerFunName, innerArgName) <- brackets $ do
        reserved "ep"
        parens $ do
            innerFunName <- identifier
            comma
            innerArgName <- identifier
            return (innerFunName, innerArgName)
    boundedArgName <- identifier
    if innerArgName /= argName || boundedArgName /= argName
        then fail "function argument names do not match"
        else return $ RecEq funName $ Recursion
            { outerFunction = outerFunName
            , innerFunction = innerFunName
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