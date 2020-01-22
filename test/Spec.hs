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
    let f = Idt "f"
    let f1 = Idt "f1"
    let f2 = Idt "f2"
    let f3 = Idt "f3"
    let one = Idt "1"
    let eqF1 = RecEq f1 (Recursion f3 one)
    let eqF2 = OpEq f2 (Operation Sub f f1)
    let eqF3 = OpEq f3 (Operation Add f1 f2)  

    let maxProgram =
            [ eqF1
            , eqF2
            , eqF3
            ]

    let validatedProgram = Map.fromList 
            [ (f1, eqF1)
            , (f2, eqF2)
            , (f3, eqF3)
            ]

    hspec $ do
        describe "Parser.Equation" $ do
            let parseEqs t = parse equations "" t
            
            it "parses a maximum program" $ do
                parseEqs maxSrc `shouldBe` (Right maxProgram)

        describe "Validator" $ do
            let malformedMaxProgram =
                    [ eqF1
                    , eqF3
                    , eqF2
                    ]

            it "collects the defined function symbols" $ do
                definedFunctions maxProgram `shouldBe` (Set.fromList [f1,f2,f3])
            it "collects the used function symbols" $ do
                usedFunctions maxProgram `shouldBe` (Set.fromList [f,f1,f2,f3,one])
            it "collects unknown function symbols" $ do
                unknownFunctions maxProgram `shouldBe` (Set.fromList [f])
            it "validates a maximum program successfully" $ do
                validate maxProgram `shouldBe` (Right validatedProgram)
            it "fails to validate a malformed maximum program" $ do
                validate malformedMaxProgram `shouldBe` (Left "invalid dependency in f3")

        describe "Interpreter" $ do
            let makeF vals = Map.fromList [(f, vals)]

            it "interprets the one function" $ do
                let twoOnes = Map.fromList [(f, OpEq f (Operation Add const1 const1))]
                interpret twoOnes Map.empty f 1 `shouldBe` 2

            it "interprets the id function" $ do
                let twoIds = Map.fromList [(f, OpEq f (Operation Add constId constId))]
                interpret twoIds Map.empty f 5 `shouldBe` 10

            it "interprets the maximum program" $ do
                interpret validatedProgram (makeF [7,2,55,13,54,11]) f3 5 `shouldBe` 55
                interpret validatedProgram (makeF [7,2,55,13,54,99]) f3 5 `shouldBe` 99
                interpret validatedProgram (makeF [107,2,55,13,54,99]) f3 5 `shouldBe` 107