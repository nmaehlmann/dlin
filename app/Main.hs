module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Parser.Equation
import Data.Bifunctor
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Text.Parsec (parse)
import System.Console.Haskeline

import AST
import Validator
import Interpreter
import Command

type Repl a = InputT IO a

data State = State
    { currentFile :: String
    , currentProgram :: Map Idt Equation
    , currentDefinitions :: Map Idt [Int]
    }

main :: IO ()
main = runInputT defaultSettings $ loop initState

loop :: State -> Repl ()
loop state = do
    minput <- getInputLine $ currentFile state ++ "> "   
    case minput of
        Nothing -> return ()
        Just ":q" -> return ()
        Just input -> handleInput state input >>= loop

handleInput :: State -> String -> Repl State
handleInput state input = do
    let parsedCommand = parse command "" input
    case parsedCommand of 
        (Left err) -> outputStrLn ("unknown command: " ++ show err) >> return state
        (Right cmd) -> handleCommand state cmd

handleCommand :: State -> Command -> Repl State
handleCommand state (Load file) = do
    srcOrException <- (lift $ try $ readFile file) :: Repl (Either SomeException String)
    let program = do
            src <- first show $ srcOrException
            eqs <- first show $ parse equations "" src
            validate eqs
    case program of 
        (Left err) -> outputStrLn ("error reading file: " ++ err) >> return state
        (Right p) -> return state {currentFile = file, currentProgram = p}

handleCommand state (Define funName funVals) = 
    return $ state {currentDefinitions = Map.insert funName funVals (currentDefinitions state)}

handleCommand state (Evaluate funName funArg) = do
    case interpret (currentProgram state) (currentDefinitions state) funName funArg of
        (Left err) -> outputStrLn err
        (Right result) -> outputStrLn $ show $ result
    return state

initState :: State
initState = State
    { currentFile = ""
    , currentProgram = Map.empty
    , currentDefinitions = Map.empty
    }