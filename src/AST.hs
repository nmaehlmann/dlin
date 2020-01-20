module AST where

data Idt = Idt String
    deriving (Eq, Show)

data Equation 
    = OpEq Idt Operation
    | RecEq Idt Recursion
    deriving (Eq, Show)

idt :: Equation -> Idt
idt (OpEq i _) = i
idt (RecEq i _) = i

data Recursion = Recursion 
    { outerFunction :: Idt
    , innerFunction :: Idt
    } deriving (Eq, Show)

data Operation = Operation
    { operator :: Operator
    , lhs :: Idt
    , rhs :: Idt
    } deriving (Eq, Show)

data Operator 
    = Add 
    | Sub 
    deriving (Eq, Show)
