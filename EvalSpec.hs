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
        
        it "errors on non-numeric inputs" $ do   
            evaluate (eval "*" [Id "string", Integer 5]) `shouldThrow` errorCall "Cannot multiply type of string"
            evaluate (eval "*" [Real 2.2, Id "string"]) `shouldThrow` errorCall "Cannot multiply type of string"
            evaluate (eval "*" [Id "string", Id "string"]) `shouldThrow` errorCall "Cannot multiply type of string"

    context "/" $ do        
        it "divides integers" $ do
            eval "/" [Integer 9, Integer 99] `shouldBe` [Integer 11]
            eval "/" [Integer (-3), Integer (-54)] `shouldBe` [Integer 18]
            eval "/" [Integer (7), Integer (33)] `shouldBe` [Integer 4]
            eval "/" [Integer (-3), Integer (52)] `shouldBe` [Integer (-17)]

        it "divides floats" $ do
            eval "/" [Integer 3, Real 4.5] `shouldBe` [Real 1.5]
            eval "/" [Real 2.0, Integer 3] `shouldBe` [Real 1.5]
            eval "/" [Real 4.0, Real (-1.0)] `shouldBe` [Real (-0.25)]

        it "errors on non-numeric inputs" $ do   
            evaluate (eval "/" [Id "string", Integer 5]) `shouldThrow` errorCall "Cannot divide type of string"
            evaluate (eval "/" [Real 2.2, Id "string"]) `shouldThrow` errorCall "Cannot divide type of string"
            evaluate (eval "/" [Id "string", Id "string"]) `shouldThrow` errorCall "Cannot divide type of string"
        
        it "errors on division by 0" $ do   
            evaluate (eval "/" [Integer 500, Integer 0]) `shouldThrow` errorCall "Cannot divide by 0"
            evaluate (eval "/" [Real 4, Real 0.0]) `shouldThrow` errorCall "Cannot divide by 0"

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

        it "errors on non-numeric inputs" $ do   
            evaluate (eval "+" [Id "string", Integer 5]) `shouldThrow` errorCall "Cannot add type of string"
            evaluate (eval "+" [Real 2.2, Id "string"]) `shouldThrow` errorCall "Cannot add type of string"
            evaluate (eval "+" [Id "string", Id "string"]) `shouldThrow` errorCall "Cannot add type of string"

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

        it "errors on non-numeric inputs" $ do   
            evaluate (eval "-" [Id "string", Integer 5]) `shouldThrow` errorCall "Cannot subtract type of string"
            evaluate (eval "-" [Real 2.2, Id "string"]) `shouldThrow` errorCall "Cannot subtract type of string"
            evaluate (eval "-" [Id "string", Id "string"]) `shouldThrow` errorCall "Cannot subtract type of string"

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

        it "errors on non-numeric inputs" $ do   
            evaluate (eval "^" [Id "string", Integer 5]) `shouldThrow` errorCall "Cannot calculate type of string"
            evaluate (eval "^" [Real 2.2, Id "string"]) `shouldThrow` errorCall "Cannot calculate type of string"
            evaluate (eval "^" [Id "string", Id "string"]) `shouldThrow` errorCall "Cannot calculate type of string"

    context "DUP" $ do
        it "duplicates values" $ do
            eval "DUP" [Integer 2] `shouldBe` [Integer 2, Integer 2]
            eval "DUP" [Real 2.2] `shouldBe` [Real 2.2, Real 2.2]
            eval "DUP" [Id "x"] `shouldBe` [Id "x", Id "x"]

        it "errors on empty stack" $ do
            evaluate (eval "DUP" []) `shouldThrow` errorCall "Stack underflow"

    context "STR" $ do
        it "converts to string" $ do
            eval "STR" [Integer 2] `shouldBe` [Id "2"]
            eval "STR" [Real 2.2] `shouldBe` [Id "2.2"]
            eval "STR" [Id "id"] `shouldBe` [Id "id"]

        it "errors on empty stack" $ do
            evaluate (eval "STR" []) `shouldThrow` errorCall "Stack underflow"

    context "CONCAT2" $ do
        it "concatenates 2 strings" $ do
            eval "CONCAT2" [Id "23", Id "50"] `shouldBe` [Id "2350"]
            eval "CONCAT2" [Id "what", Id "a", Id "day"] `shouldBe` [Id "whata", Id "day"]

        it "throws error when arguments are not of type String" $ do
            evaluate (eval "CONCAT2" [Real 4.4, Id "50"]) `shouldThrow` errorCall("Arguments are not of type String")
            evaluate (eval "CONCAT2" [Integer 0, Real 1.1]) `shouldThrow` errorCall("Arguments are not of type String")

        it "errors on empty stack" $ do
            evaluate (eval "CONCAT2" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "CONCAT2" [Id "test"]) `shouldThrow` errorCall "Stack underflow"

    context "CONCAT3" $ do
        it "concatenates 3 strings" $ do
            eval "CONCAT3" [Id "23", Id "50", Id "99"] `shouldBe` [Id "235099"]
            eval "CONCAT3" [Id "what", Id "a", Id "day", Id "!"] `shouldBe` [Id "whataday", Id "!"]

        it "throws error when arguments are not of type String" $ do
            evaluate (eval "CONCAT3" [Id "id", Id "50", Integer 5]) `shouldThrow` errorCall("Arguments are not of type String")
            evaluate (eval "CONCAT3" [Integer 0, Real 1.1, Real 0.0]) `shouldThrow` errorCall("Arguments are not of type String")

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
            evalOut "EMIT" ([Integer 65], "") `shouldBe` ([],"A")
            evalOut "EMIT" ([Integer 88], "") `shouldBe` ([], "X")

        it "errors when Integer out of range" $ do
            evaluate (evalOut "EMIT" ([Integer 128], "")) `shouldThrow` errorCall("ASCII code out of range")
            evaluate (evalOut "EMIT" ([Integer (-1)], "")) `shouldThrow` errorCall("ASCII code out of range")

        it "errors when argument type is not Integer" $ do
            evaluate (evalOut "EMIT" ([Real 2.2], "")) `shouldThrow` errorCall("Arguments are not of type Int")
            evaluate (evalOut "EMIT" ([Id "string"], "")) `shouldThrow` errorCall("Arguments are not of type Int")

        it "errors on empty stack" $ do
            evaluate (evalOut "EMIT" ([], "")) `shouldThrow` errorCall "Stack underflow"

      context "CR" $ do
        it "prints a new line" $ do
          evalOut "CR" ([], "") `shouldBe` ([], "\n")
          evalOut "CR" ([Real 2.0, Integer 5], "") `shouldBe` ([Real 2.0, Integer 5], "\n")

      it "eval pass-through" $ do
         evalOut "*" ([Real 2.0, Integer 2], "blah") `shouldBe` ([Real 4.0], "blah") 
