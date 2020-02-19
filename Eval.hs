module Eval where
-- This file contains definitions for functions and operators

import Val
import Data.Char 

-- main evaluation function for operators and 
-- built-in FORTH functions with no output
-- takes a string and a stack and returns the stack
-- resulting from evaluation of the function
eval :: String -> [Val] -> [Val]
-- Multiplication
eval "*" (Id x : _ : tl) = error("Cannot multiply type of string")
eval "*" (_ : Id x : tl) = error("Cannot multiply type of string")
-- if arguments are integers, keep result as integer
eval "*" (Integer x: Integer y:tl) = Integer (x*y) : tl
-- if any argument is float, make result a float
eval "*" (x:y:tl) = (Real $ toFloat x * toFloat y) : tl 
-- any remaining cases are stacks too short
eval "*" _ = error("Stack underflow")

-- Division
eval "/" (Id x : _ : tl) = error("Cannot divide type of string")
eval "/" (_ : Id x : tl) = error("Cannot divide type of string")
eval "/" (_ : Integer 0 : tl) = error("Cannot divide by 0")
eval "/" (_ : Real 0.0 : tl) = error("Cannot divide by 0")
eval "/" (Integer x: Integer y:tl) = Integer (truncate (fromIntegral y / fromIntegral x)) : tl
eval "/" (x : y : tl) = (Real $ toFloat y / toFloat x) : tl 
eval "/" _ = error("Stack underflow")

-- Addition
eval "+" (Id x : _ : tl) = error("Cannot add type of string")
eval "+" (_ : Id x : tl) = error("Cannot add type of string")
eval "+" (Integer x : Integer y : tl) = Integer(x + y) : tl
eval "+" (x : y : tl) = (Real $ toFloat x + toFloat y) : tl
eval "+" _ = error("Stack underflow")

-- Subtraction
eval "-" (Id x : _ : tl) = error("Cannot subtract type of string")
eval "-" (_ : Id x : tl) = error("Cannot subtract type of string")
eval "-" (Integer x : Integer y : tl) = Integer(y - x) : tl
eval "-" (x : y : tl) = (Real $ toFloat y - toFloat x) : tl
eval "-" _ = error("Stack underflow")

-- Power function
eval "^" (Id x : _ : tl) = error("Cannot calculate type of string")
eval "^" (_ : Id x : tl) = error("Cannot calculate type of string")
eval "^" (Integer x : Integer y : tl)
  | x >= 0 = Integer(y ^ x) : tl
  | otherwise = error("Negative exponent exception")
eval "^" (x : y : tl) = (Real $ toFloat y ** toFloat x) : tl
eval "^" _ = error("Stack underflow")

-- Duplicate the element at the top of the stack
eval "DUP" (x:tl) = (x:x:tl)
eval "DUP" [] = error("Stack underflow")

-- Converts element to string
eval "STR" (Id x : tl) = (Id x : tl)
eval "STR" (Integer x : tl) = (Id (show x) : tl)
eval "STR" (Real x : tl) = (Id (show x) : tl)
eval "STR" [] = error("Stack underflow")

-- Concatenate 2 strings from the stack
eval "CONCAT2" (Id x : Id y : tl) = Id (x ++ y) : tl
eval "CONCAT2" (x : y : tl) = error("Arguments are not of type String")
eval "CONCAT2" _ = error("Stack underflow")

-- Concatenate 3 strings from the stack
eval "CONCAT3" (Id x : Id y : Id z : tl) = Id (x ++ y ++ z) : tl
eval "CONCAT3" (x : y : z : tl) = error("Arguments are not of type String")
eval "CONCAT3" _ = error("Stack underflow")

-- this must be the last rule
-- it assumes that no match is made and preserves the string as argument
eval s l = Id s : l 


-- variant of eval with output
-- state is a stack and string pair
evalOut :: String -> ([Val], String) -> ([Val], String) 
-- print element at the top of the stack
evalOut "." (Id x:tl, out) = (tl, out ++ x)
evalOut "." (Integer i:tl, out) = (tl, out ++ (show i))
evalOut "." (Real x:tl, out) = (tl, out ++ (show x))
evalOut "." ([], _) = error "Stack underflow"

-- Outputs correcponding ascii character
evalOut "EMIT" (Integer i : tl, out)
  | i >= 0 && i < 128 = (tl, out ++ (charToString (chr i)))
  | otherwise = error("ASCII code out of range")
evalOut "EMIT" (x : tl, out) = error("Arguments are not of type Int")
evalOut "EMIT" ([], _) = error "Stack underflow"

-- prints a new line
evalOut "CR" (s, out) = (s, out ++ "\n")

-- this has to be the last case
-- if no special case, ask eval to deal with it and propagate output
evalOut op (stack, out) = (eval op stack, out)
