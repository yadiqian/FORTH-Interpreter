module Eval where
-- This file contains definitions for functions and operators

import Val

-- main evaluation function for operators and 
-- built-in FORTH functions with no output
-- takes a string and a stack and returns the stack
-- resulting from evaluation of the function
eval :: String -> [Val] -> [Val]
-- Multiplication
-- if arguments are integers, keep result as integer
eval "*" (Integer x: Integer y:tl) = Integer (x*y) : tl
-- if any argument is float, make result a float
eval "*" (x:y:tl) = (Real $ toFloat x * toFloat y) : tl 
-- any remaining cases are stacks too short
eval "*" _ = error("Stack underflow")

-- Division
eval "/" (x : y : tl)
  | result == Real (fromIntegral toInt) = Integer toInt : tl
  | otherwise = result : tl
  where result = Real $ toFloat y / toFloat x
        toInt = round (toFloat result)

eval "/" _ = error("Stack underflow")

-- Addition
eval "+" (Integer x : Integer y : tl) = Integer(x + y) : tl
eval "+" (x : y : tl) = (Real $ toFloat x + toFloat y) : tl
eval "+" _ = error("Stack underflow")

-- Subtraction
eval "-" (Integer x : Integer y : tl) = Integer(y - x) : tl
eval "-" (x : y : tl) = (Real $ toFloat y - toFloat x) : tl
eval "-" _ = error("Stack underflow")


-- Duplicate the element at the top of the stack
eval "DUP" (x:tl) = (x:x:tl)
eval "DUP" [] = error("Stack underflow")

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

-- this has to be the last case
-- if no special case, ask eval to deal with it and propagate output
evalOut op (stack, out) = (eval op stack, out)
