{-# LANGUAGE DerivingVia #-}
module Interpreter where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as StateT
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array (Array)
import qualified Data.Array as Array

import AST

newtype IxIdt = IxIdt Int
    deriving (Eq, Ord, Show, Array.Ix) via Int

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
    , functionDictionary :: Map IxIdt IxFun
    , cache :: Array (IxIdt, Int) (Maybe Int)
    }

cacheLookup :: IxIdt -> Int -> InterpreterVal (Maybe Int)
cacheLookup fIdt x = do
    c <- cache <$> StateT.get 
    return $ c Array.! (fIdt, x)

cacheWrite :: IxIdt -> Int -> Int -> InterpreterVal ()
cacheWrite fIdt x y = do
    let updCache = \c -> c Array.// [((fIdt, x), Just y)]
    let updState = \s -> s {cache = updCache (cache s)}
    StateT.modify updState

interpret :: Map Idt Equation -> Map Idt [Int] -> Int -> Idt -> Int -> Either String Int
interpret definitions predefineds magnificationBound f x = do
    n <- lookupN predefineds
    let uB = n * magnificationBound
    (idtIndex, functionIndex) <- indexDictionary $ buildDictionary definitions predefineds
    let maxIdtIdx = length $ Map.keys idtIndex
    let emptyCache = Array.listArray ((IxIdt 0, 0), (IxIdt maxIdtIdx, uB)) (replicate (maxIdtIdx * uB) Nothing)
    let initialState = InterpreterState uB functionIndex emptyCache
    let ixF = idtIndex Map.! f
    StateT.evalStateT (evaluate ixF x) initialState

lookupN :: Map Idt [Int] -> Either String Int 
lookupN predefineds = case Map.lookup constN predefineds of
    (Just (n : _)) -> return n
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

evaluate :: IxIdt -> Int -> Interpreter
evaluate fIdt x = do
    validInput <- inBounds x
    if validInput
        then do
            cachedResult <- cacheLookup fIdt x
            case cachedResult of
                Nothing -> do
                    dictionary <- functionDictionary <$> StateT.get
                    f <- case Map.lookup fIdt dictionary of
                        (Just f) -> return f
                        Nothing -> lift $ Left $ "Error: undefined function symbol " ++ show fIdt
                    result <- evaluateUnsafe f x
                    validOutput <- inBounds result
                    let result = if validOutput then result else 0
                    cacheWrite fIdt x result
                    return result
                (Just result) -> return result
        else return 0

evaluateUnsafe :: IxFun -> Int -> Interpreter
evaluateUnsafe IxConstOne _ = return 1
evaluateUnsafe IxIdentity x = return x
evaluateUnsafe (IxPredefined vals) x = return $ vals !! x
evaluateUnsafe (IxOpFun op lhs rhs) x = do
    lhsResult <- evaluate lhs x
    rhsResult <- evaluate rhs x
    return $ interpretOperator op lhsResult rhsResult
evaluateUnsafe (IxRecFun outerFun innerFun) x = do
    eP <- equalPredecessor innerFun x
    boundedApplication outerFun eP x

equalPredecessor :: IxIdt -> Int -> Interpreter
equalPredecessor f x = do
    r <- evaluate f x
    let eP (-1) = return x
        eP i    = do
            r' <- evaluate f i
            if r' == r 
                then return i
                else eP (i-1)
    eP (x-1)

boundedApplication :: IxIdt -> Int -> Int -> Interpreter
boundedApplication f x bound = if x < bound 
    then evaluate f x
    else return x

interpretOperator :: Operator -> Int -> Int -> Int
interpretOperator Add = (+)
interpretOperator Sub = (-)