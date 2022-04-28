module Main where

import Parsing

-- expr ::= term + expr | term
-- term ::= factor * term | factor
-- expr ::= (expr) | int
main :: IO ()
main = do
  putStrLn "hello world"

expr = do 
    x <- term
    char '+'
    y <- expr
    return (x + y)
    <|> term 


term = do
    x <- factor
    char '*'
    y <- term
    return (x * y)
    <|> factor 

factor = do
    char '('
    x <- expr
    char ')'
    return x  
    <|> int

test = parse expr "2+3*4"
testt = parse expr "(2+3)*4"
testtt = parse expr "(2+(3*4)+7)*10"