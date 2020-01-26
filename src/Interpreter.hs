module Interpreter where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as StateT
import Data.Map (Map)
import qualified Data.Map as Map

import AST

data IxIdt = IxIdt Int
    deriving (Eq, Ord)

data IxFun 
    = IxOpFun Operator IxIdt IxIdt
    | IxRecFun IxIdt IxIdt
    | IxPredefined [Int]
    | IxConstOne
    | IxIdentity

data Fun
    = OpFun Operator Idt Idt
    | RecFun Idt Idt
    | Predefined [Int]
    | ConstOne
    | Identity

indexIdentifiers :: [Idt] -> Map Idt IxIdt
indexIdentifiers idts = Map.fromList $ zip idts $ map IxIdt [0..]

lookupIdt :: Map Idt IxIdt -> Idt -> Either String IxIdt
lookupIdt dict idt = case Map.lookup idt dict of
    (Just ixIdt) -> Right ixIdt
    Nothing -> Left $ "Error: unknown symbol " ++ show idt

indexFunction :: Map Idt IxIdt -> Fun -> Either String IxFun
indexFunction dict fun = let getIx = lookupIdt dict 
    in case fun of
            (OpFun op idt1 idt2) -> do
                ixIdt1 <- getIx idt1
                ixIdt2 <- getIx idt2
                return $ IxOpFun op ixIdt1 ixIdt2
            (RecFun idt1 idt2) -> do
                ixIdt1 <- getIx idt1
                ixIdt2 <- getIx idt2
                return $ IxRecFun ixIdt1 ixIdt2
            (Predefined vals) -> return $ IxPredefined vals
            ConstOne -> return IxConstOne
            Identity -> return IxIdentity

indexDictionary :: Map Idt Fun -> Either String (Map Idt IxIdt, Map IxIdt IxFun)
indexDictionary dict = do
    let idtIndex = indexIdentifiers $ Map.keys dict
    let indexedIdts = Map.elems idtIndex
    indexedFuns <- sequence $ map (indexFunction idtIndex) $ Map.elems dict
    let funIndex = Map.fromList $ zip indexedIdts indexedFuns
    return $ (idtIndex, funIndex)

type InterpreterVal a = StateT InterpreterState (Either String) a
type Interpreter = InterpreterVal Int

data InterpreterState = InterpreterState 
    { upperBound :: Int
    , functionDictionary :: Map Idt Fun
    , cache :: Map (Idt, Int) Int
    }

interpret' :: Map Idt Equation -> Map Idt [Int] -> Int -> Idt -> Int -> Either String Int
interpret' definitions predefineds magnificationBound f x = do
    n <- lookupN predefineds
    let uB = n * magnificationBound
    (idtIndex, functionIndex) <- indexDictionary $ buildDictionary definitions predefineds 
    return 1

lookupN :: Map Idt [Int] -> Either String Int 
lookupN predefineds = case Map.lookup constN predefineds of
    (Just (n : _)) -> return n
    otherwise -> Left $ "Error: undefined function symbol " ++ show constN

interpret :: Map Idt Equation -> Map Idt [Int] -> Int -> Idt -> Int -> Either String Int
interpret definitions predefineds magnificationBound f x = case Map.lookup constN predefineds of
    (Just (n : _)) -> let 
            dict = buildDictionary definitions predefineds
            uB = n * magnificationBound
            initialState = InterpreterState uB dict Map.empty
        in StateT.evalStateT (evaluate f x) initialState
    otherwise -> Left $ "Error: undefined function symbol " ++ show constN

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

inBounds :: Int -> InterpreterVal Bool
inBounds x = do 
    uB <- upperBound <$> StateT.get
    return $ 0 <= x && x < uB - 1

evaluate :: Idt -> Int -> Interpreter
evaluate fIdt x = do
    validInput <- inBounds x
    if validInput
        then do
            dictionary <- functionDictionary <$> StateT.get
            f <- case Map.lookup fIdt dictionary of
                    (Just f) -> return f
                    Nothing -> lift $ Left $ "Error: undefined function symbol " ++ show fIdt
            result <- evaluateUnsafe f x
            validOutput <- inBounds result
            if validOutput 
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