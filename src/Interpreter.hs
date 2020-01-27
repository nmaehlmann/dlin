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
    | IxPredefined (Array Int Int)
    | IxConstOne
    | IxIdentity
    deriving Show

data Fun
    = OpFun Operator Idt Idt
    | RecFun Idt Idt
    | Predefined [Int]
    | ConstOne
    | Identity
    deriving Show

type InterpreterVal a = StateT InterpreterState (Either String) a

type Interpreter = InterpreterVal Int

data InterpreterState = InterpreterState 
    { upperValueBound :: Int
    , functionDictionary :: Map IxIdt IxFun
    , cache :: Array (IxIdt, Int) (Maybe Int)
    }
    deriving Show

indexIdentifiers :: [Idt] -> Map Idt IxIdt
indexIdentifiers idts = Map.fromList $ zip idts $ map IxIdt [0..100]

lookupIdt :: Map Idt IxIdt -> Idt -> Either String IxIdt
lookupIdt dict idt = case Map.lookup idt dict of
    (Just ixIdt) -> Right ixIdt
    Nothing -> Left $ "Error: unknown symbol " ++ show idt

indexFunction :: Int -> Map Idt IxIdt -> Fun -> Either String IxFun
indexFunction uB dict fun = let getIx = lookupIdt dict 
    in case fun of
            (OpFun op idt1 idt2) -> do
                ixIdt1 <- getIx idt1
                ixIdt2 <- getIx idt2
                return $ IxOpFun op ixIdt1 ixIdt2
            (RecFun idt1 idt2) -> do
                ixIdt1 <- getIx idt1
                ixIdt2 <- getIx idt2
                return $ IxRecFun ixIdt1 ixIdt2
            (Predefined vals) -> return $ IxPredefined $ Array.listArray (0, uB - 1) vals
            ConstOne -> return IxConstOne
            Identity -> return IxIdentity

indexDictionary :: Int -> Map Idt Fun -> Either String (Map Idt IxIdt, Map IxIdt IxFun)
indexDictionary uB dict = do
    let idtIndex = indexIdentifiers $ Map.keys dict
    let indexedIdts = Map.elems idtIndex
    indexedFuns <- sequence $ map (indexFunction uB idtIndex) $ Map.elems dict
    let funIndex = Map.fromList $ zip indexedIdts indexedFuns
    return $ (idtIndex, funIndex)

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
interpret definitions predefineds uB f x = do
    let dict = buildDictionary definitions predefineds
    (idtIndex, functionIndex) <- indexDictionary uB dict 
    let maxIdtIdx = length $ Map.keys idtIndex
    let lowerArrayBound = (IxIdt 0, 0)
    let upperArrayBound = (IxIdt (maxIdtIdx - 1), uB - 1)
    let emptyCache = Array.listArray (lowerArrayBound, upperArrayBound) $ replicate (maxIdtIdx * uB) Nothing
    let initialState = InterpreterState uB functionIndex emptyCache
    ixF <- lookupIdt idtIndex f
    StateT.evalStateT (evaluate ixF x) initialState

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
    uB <- upperValueBound <$> StateT.get
    return $ 0 <= x && x < uB

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
                    evaluationResult <- evaluateUnsafe f x
                    validOutput <- inBounds evaluationResult
                    let result = if validOutput then evaluationResult else 0
                    cacheWrite fIdt x result
                    return result
                (Just result) -> return result
        else return 0

evaluateUnsafe :: IxFun -> Int -> Interpreter
evaluateUnsafe IxConstOne _ = return 1
evaluateUnsafe IxIdentity x = return x
evaluateUnsafe (IxPredefined vals) x = return $ vals Array.! x
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