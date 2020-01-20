import Test.Hspec
import Text.Parsec
import Data.Either
import AST
import Parser.Equation
import Data.Set as Set
import Validator

main :: IO ()
main = do
    maxSrc <- readFile "example/maximum.dlin"
    let f = Idt "f"
    let f1 = Idt "f1"
    let f2 = Idt "f2"
    let f3 = Idt "f3"
    let one = Idt "1"
    let expectedProgram =
            [ RecEq f1 (Recursion f3 one)
            , OpEq f2 (Operation Sub f f1)
            , OpEq f3 (Operation Add f1 f2)
            ] 

    hspec $ do
        describe "Parser.Equation" $ do
            let parseEqs t = parse equations "" t
            it "parses a maximum program" $ do
                parseEqs maxSrc `shouldBe` (Right expectedProgram)

        describe "Validator" $ do
            it "collects the defined function symbols" $ do
                definedFunctions expectedProgram `shouldBe` (Set.fromList [f1,f2,f3])
            it "collects the used function symbols" $ do
                usedFunctions expectedProgram `shouldBe` (Set.fromList [f,f1,f2,f3,one])
            it "collects unknown function symbols" $ do
                unknownFunctions expectedProgram `shouldBe` (Set.fromList [f])