# Forth Interpreter

All required functionalities are implemented. 

### Running unit tests

```
runhaskell ValSpec.hs
runhaskell EvalSpec.hs
runhaskell InterpretSpec.hs
```

### Running functional tests
```
cabal run tests/t1.4TH
```
up till ```tests/t10.4TH``` to run each functional test

### Notes
Whenever there is a type mismatch such as adding an integer with a string or stack underflow etc., the program will print out an error message and stop the program. 