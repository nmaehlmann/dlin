module Interpreter where

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map

import AST

data Fun 
    = OpFun Operator Idt Idt
    | RecFun Idt Idt
    | Predefined [Int]
    | ConstOne
    | Identity

type Interpreter = ReaderT (Map Idt Fun) (Either String) Int

interpret :: Map Idt Equation -> Map Idt [Int] -> Idt -> Int -> Either String Int
interpret definitions predefineds f x = runReaderT (evaluate f x) dict
    where dict = buildDictionary definitions predefineds

buildDictionary :: Map Idt Equation -> Map Idt [Int] -> Map Idt Fun
buildDictionary definitions predefineds = addConstOne $ addIdentity $ Map.union mappedDefinitions mappedPredefineds
    where 
        mappedDefinitions = Map.map eqToFun definitions
        mappedPredefineds = Map.map predefinedToFun predefineds
        addConstOne = Map.insert const1 ConstOne
        addIdentity = Map.insert constId Identity

eqToFun :: Equation -> Fun
eqToFun (OpEq _ (Operation op lhs rhs)) = OpFun op lhs rhs
eqToFun (RecEq _ (Recursion outer inner)) = RecFun outer inner

predefinedToFun :: [Int] -> Fun
predefinedToFun rs = Predefined $ rs ++ repeat 0

inBounds :: Int -> Bool
inBounds x = 0 <= x && x < upperBound - 1
    where upperBound = 1000

evaluate :: Idt -> Int -> Interpreter
evaluate fIdt x = if inBounds x
    then do
        dictionary <- ask
        f <- case Map.lookup fIdt dictionary of
                (Just f) -> return f
                Nothing -> lift $ Left $ "Error: undefined function symbol " ++ show fIdt
        result <- evaluateUnsafe f x
        if inBounds result 
            then return result
            else return 0
    else return 0

evaluateUnsafe :: Fun -> Int -> Interpreter
evaluateUnsafe ConstOne _ = return 1
evaluateUnsafe Identity x = return x
evaluateUnsafe (Predefined vals) x = return $ vals !! x
evaluateUnsafe (OpFun op lhs rhs) x = do
    lhsResult <- evaluate lhs x
    rhsResult <- evaluate rhs x
    return $ interpretOperator op lhsResult rhsResult
evaluateUnsafe (RecFun outerFun innerFun) x = do
    eP <- equalPredecessor innerFun x
    boundedApplication outerFun eP x

equalPredecessor :: Idt -> Int -> Interpreter
equalPredecessor f x = do
    r <- evaluate f x
    let eP (-1) = return x
        eP i    = do
            r' <- evaluate f i
            if r' == r 
                then return i
                else eP (i-1)
    eP (x-1)

boundedApplication :: Idt -> Int -> Int -> Interpreter
boundedApplication f x bound = if x < bound 
    then evaluate f x
    else return x

interpretOperator :: Operator -> Int -> Int -> Int
interpretOperator Add = (+)
interpretOperator Sub = (-)