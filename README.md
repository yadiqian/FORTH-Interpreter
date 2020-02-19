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
Whenever there is a type mismatch such as adding an integer with a string or stack underflow etc., the program will print out an error message and stop the program. Some special cases considered are: 

* Dividing by Integer 0 or Real 0.0 should not be allowed
* Negative exponent in power function when both arguments are integers should not be allowed since that will create a non-integer result
* ```EMIT``` function should only allow integer values from 0 to 127 since those are the valid ASCII codes

Also, the orignial problematic test ```errors on non-numeric inputs``` is fixed in my assignment.