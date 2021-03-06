import Test.Hspec
import Text.Parsec
import Data.Either
import AST
import Parser.Equation
import Data.Set as Set
import Data.Map as Map
import Validator
import Interpreter

main :: IO ()
main = do
    maxSrc <- readFile "examples/maximum.dlin"
    let input = Idt "input"
    let pmax = Idt "pmax"
    let i = Idt "i"
    let max = Idt "max"
    let one = Idt "1"
    let eqPMax = RecEq pmax (Recursion max one)
    let eqI = OpEq i (Operation Sub input pmax)
    let eqMax = OpEq max (Operation Add pmax i)

    let maxProgram =
            [ eqPMax
            , eqI
            , eqMax
            ]

    let validatedProgram = Map.fromList 
            [ (pmax, eqPMax)
            , (i, eqI)
            , (max, eqMax)
            ]

    hspec $ do
        describe "Parser.Equation" $ do
            let parseEqs t = parse equations "" t
            
            it "parses a maximum program" $ do
                parseEqs maxSrc `shouldBe` (Right maxProgram)

        describe "Validator" $ do
            let malformedMaxProgram =
                    [ eqPMax
                    , eqMax
                    , eqI
                    ]

            it "collects the defined function symbols" $ do
                definedFunctions maxProgram `shouldBe` (Set.fromList [pmax,i,max])
            it "collects the used function symbols" $ do
                usedFunctions maxProgram `shouldBe` (Set.fromList [input,pmax,i,max,one])
            it "collects unknown function symbols" $ do
                unknownFunctions maxProgram `shouldBe` (Set.fromList [input])
            it "validates a maximum program successfully" $ do
                validate maxProgram `shouldBe` (Right validatedProgram)
            it "fails to validate a malformed maximum program" $ do
                validate malformedMaxProgram `shouldBe` (Left "invalid dependency in max")

        describe "Interpreter" $ do
            let makeInput vals = Map.fromList [(input, vals)]

            it "interprets the one function" $ do
                let twoOnes = Map.fromList [(input, OpEq input (Operation Add const1 const1))]
                interpret twoOnes Map.empty input 5 `shouldBe` (Right 2)

            it "interprets the id function" $ do
                let twoIds = Map.fromList [(input, OpEq input (Operation Add constId constId))]
                interpret twoIds Map.empty input 5 `shouldBe` (Right 10)

            it "interprets the maximum program" $ do
                interpret validatedProgram (makeInput [7,2,55,13,54,11]) max 5 `shouldBe` (Right 55)
                interpret validatedProgram (makeInput [7,2,55,13,54,99]) max 5 `shouldBe` (Right 99)
                interpret validatedProgram (makeInput [107,2,55,13,54,99]) max 5 `shouldBe` (Right 107)