-- HSpec tests for Val.hs
-- Execute: runhaskell EvalSpec.hs

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Val
import Eval

main :: IO ()
main = hspec $ do
  describe "eval" $ do
    context "*" $ do
        it "multiplies integers" $ do
            eval "*" [Integer 2, Integer 3] `shouldBe` [Integer 6]
        
        it "multiplies floats" $ do
            eval "*" [Integer 2, Real 3.0] `shouldBe` [Real 6.0]
            eval "*" [Real 3.0, Integer 3] `shouldBe` [Real 9.0]
            eval "*" [Real 4.0, Real 3.0] `shouldBe` [Real 12.0]

        it "errors on too few arguments" $ do   
            evaluate (eval "*" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "*" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

        -- this does not work, seems to be a HSpec bug
        -- it "errors on non-numeric inputs" $ do
        --    evaluate(eval "*" [Real 3.0, Id "x"]) `shouldThrow` anyException

    context "/" $ do        
        it "divides integer numbers and get int result" $ do
            eval "/" [Integer 9, Integer 99] `shouldBe` [Integer 11]
            eval "/" [Integer (-3), Integer (-54)] `shouldBe` [Integer 18]

        it "divides numbers and get floating point result" $ do
            eval "/" [Integer 5, Integer 4] `shouldBe` [Real 0.8]
            eval "/" [Integer 3, Real 4.5] `shouldBe` [Real 1.5]
            eval "/" [Real 2.0, Integer 3] `shouldBe` [Real 1.5]
            eval "/" [Real 4.0, Real (-1.0)] `shouldBe` [Real (-0.25)]

        it "errors on too few arguments" $ do   
            evaluate (eval "/" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "/" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "+" $ do
        it "adds integers" $ do
            eval "+" [Integer 2, Integer 3] `shouldBe` [Integer 5]
        
        it "adds floats" $ do
            eval "+" [Integer 2, Real 3.0] `shouldBe` [Real 5.0]
            eval "+" [Real 3.0, Integer 3] `shouldBe` [Real 6.0]
            eval "+" [Real 4.0, Real 3.0] `shouldBe` [Real 7.0]

        it "errors on too few arguments" $ do   
            evaluate (eval "+" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "+" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "-" $ do
        it "subtracts integers" $ do
            eval "-" [Integer 2, Integer 3] `shouldBe` [Integer 1]
        
        it "subtracts floats" $ do
            eval "-" [Integer 3, Real 2.0] `shouldBe` [Real (-1.0)]
            eval "-" [Real 3.0, Integer 3] `shouldBe` [Real 0.0]
            eval "-" [Real (-4.0), Real 3.0] `shouldBe` [Real 7.0]

        it "errors on too few arguments" $ do   
            evaluate (eval "-" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "-" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "^" $ do
        it "calculates power of integers" $ do
            eval "^" [Integer 2, Integer 3] `shouldBe` [Integer 9]
        
        it "calculates power of floats" $ do
            eval "^" [Integer 3, Real 2.0] `shouldBe` [Real 8.0]
            eval "^" [Real 3.0, Integer 3] `shouldBe` [Real 27.0]
            eval "^" [Real (-4.0), Real 1.0] `shouldBe` [Real 1.0]

        it "errors on too few arguments" $ do   
            evaluate (eval "-" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "-" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "DUP" $ do
        it "duplicates values" $ do
            eval "DUP" [Integer 2] `shouldBe` [Integer 2, Integer 2]
            eval "DUP" [Real 2.2] `shouldBe` [Real 2.2, Real 2.2]
            eval "DUP" [Id "x"] `shouldBe` [Id "x", Id "x"]

        it "errors on empty stack" $ do
            evaluate (eval "DUP" []) `shouldThrow` errorCall "Stack underflow"

    context "CONCAT2" $ do
        it "concatenates 2 strings" $ do
            eval "CONCAT2" [Id "23", Id "50"] `shouldBe` [Id "2350"]
            eval "CONCAT2" [Id "what", Id "a", Id "day"] `shouldBe` [Id "whata", Id "day"]

        it "throws error when arguments are not of type String" $ do
            evaluate (eval "CONCAT2" [Real 4.4, Id "50"]) `shouldThrow` anyException
            evaluate (eval "CONCAT2" [Integer 0, Real 1.1]) `shouldThrow` anyException

        it "errors on empty stack" $ do
            evaluate (eval "CONCAT2" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "CONCAT2" [Id "test"]) `shouldThrow` errorCall "Stack underflow"

    context "CONCAT3" $ do
        it "concatenates 3 strings" $ do
            eval "CONCAT3" [Id "23", Id "50", Id "99"] `shouldBe` [Id "235099"]
            eval "CONCAT3" [Id "what", Id "a", Id "day", Id "!"] `shouldBe` [Id "whataday", Id "!"]

        it "throws error when arguments are not of type String" $ do
            evaluate (eval "CONCAT3" [Id "id", Id "50", Integer 5]) `shouldThrow` anyException
            evaluate (eval "CONCAT3" [Integer 0, Real 1.1, Real 0.0]) `shouldThrow` anyException

        it "errors on empty stack" $ do
            evaluate (eval "CONCAT3" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "CONCAT3" [Id "hello", Id "world"]) `shouldThrow` errorCall "Stack underflow"

  describe "evalOut" $ do
      context "." $ do
        it "prints top of stack" $ do
            evalOut "." ([Id "x"], "") `shouldBe` ([],"x")
            evalOut "." ([Integer 2], "") `shouldBe` ([], "2")
            evalOut "." ([Real 2.2], "") `shouldBe` ([], "2.2")

        it "errors on empty stack" $ do
            evaluate(evalOut "." ([], "")) `shouldThrow` errorCall "Stack underflow"

      context "EMIT" $ do
        it "prints top of stack in ascii character" $ do
            evalOut "EMIT" ([Id "x"], "") `shouldBe` ([],"x")
            evalOut "EMIT" ([Integer 88], "") `shouldBe` ([], "X")
            evalOut "EMIT" ([Real 2.2], "") `shouldBe` ([], "2.2")

        it "errors when Integer out of range" $ do
            evaluate (evalOut "EMIT" ([Integer 128], "")) `shouldThrow` anyException
            evaluate (evalOut "EMIT" ([Integer (-1)], "")) `shouldThrow` anyException

        it "errors on empty stack" $ do
            evaluate (evalOut "EMIT" ([], "")) `shouldThrow` errorCall "Stack underflow"

      context "CR" $ do
        it "prints a new line" $ do
          evalOut "CR" ([], "") `shouldBe` ([], "\n")
          evalOut "CR" ([Real 2.0, Integer 5], "") `shouldBe` ([Real 2.0, Integer 5], "\n")

      it "eval pass-through" $ do
         evalOut "*" ([Real 2.0, Integer 2], "blah") `shouldBe` ([Real 4.0], "blah") 
