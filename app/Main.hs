module Main where

import System.Console.Haskeline
import Text.Parsec (parse)

import AST
import Validator
import Interpreter
import Data.Map (Map)
import qualified Data.Map as Map
import Command
import Parser.Equation
import Data.Bifunctor
import Control.Monad
import Control.Monad.Trans.Class

main :: IO ()
main = runInputT defaultSettings $ loop initState

loop :: State -> InputT IO ()
loop state = do
    minput <- getInputLine $ currentFile state ++ "> "   
    case minput of
        Nothing -> return ()
        Just ":q" -> return ()
        Just input -> handleInput state input >>= loop

handleInput :: State -> String -> InputT IO State
handleInput state input = do
    let parsedCommand = parse command "" input
    case parsedCommand of 
        (Left err) -> outputStrLn ("unknown command: " ++ show err) >> return state
        (Right cmd) -> handleCommand state cmd

handleCommand :: State -> Command -> InputT IO State
handleCommand state (Load file) = do
    src <- lift $ readFile file
    let program = do
            eqs <- first show $ parse equations "" src
            validate eqs
    case program of 
        (Left err) -> outputStrLn ("error reading file: " ++ err) >> return state
        (Right p) -> return state {currentFile = file, currentProgram = p}
handleCommand state (Define funName funVals) = 
    return $ state {currentDefinitions = Map.insert funName funVals (currentDefinitions state)}
handleCommand state (Evaluate funName funArg) = do
    outputStrLn $ show $ interpret (currentProgram state) (currentDefinitions state) funName funArg
    return state

data State = State
    { currentFile :: String
    , currentProgram :: Map Idt Equation
    , currentDefinitions :: Map Idt [Int]
    }

initState :: State
initState = State
    { currentFile = ""
    , currentProgram = Map.empty
    , currentDefinitions = Map.empty
    }