module Main where
import HaskellSay (haskellSay)
import Parsing
import App
import Control.Applicative
import Data.Char

-- expr ::= term + expr | term
-- term ::= factor * term | factor
-- expr ::= (expr) | int
fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n-1)
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
main :: IO ()
main = do
  haskellSay "hello world"

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