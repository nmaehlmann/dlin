module Validator where

import Data.Set (Set)
import qualified Data.Set as Set

import AST
import Parser.Equation

predefinedFunctions :: Set Idt
predefinedFunctions = Set.fromList [Idt "1", Idt "id", Idt "n"]

unknownFunctions :: [Equation] -> Set Idt
unknownFunctions eqs = Set.difference (usedFunctions eqs) $ Set.union predefinedFunctions $ definedFunctions eqs

definedFunctions :: [Equation] -> Set Idt
definedFunctions eqs = Set.fromList $ map idt eqs

usedFunctions :: [Equation] -> Set Idt
usedFunctions eqs = Set.fromList $ concatMap usedFunctions' eqs
    where 
        usedFunctions' (OpEq _ operation) = 
            [lhs, rhs] <*> [operation]
        usedFunctions' (RecEq _ recursion) = 
            [outerFunction, innerFunction] <*> [recursion]