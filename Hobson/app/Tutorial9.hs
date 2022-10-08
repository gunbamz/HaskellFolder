module Tutorial9 where 

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where 
    fmap f (Parser x) = Parser $ \s -> do
        (x',s') <- x s
        return (f x', s')

instance Applicative Parser where 
     pure x = Parser $ \s -> Just (x,s)
     (Parser f) <*> (Parser x) = Parser $ \s -> do
         (f', s1) <- f s
         (x', s2) <- x s1
         return (f' x', s2) 

instance Monad Parser where
    (Parser x) >>= f = Parser $ \s -> do
        (x', s') <- x s
        runParser (f x') s'

instance MonadFail Parser where 
    fail _ = Parser $ \s -> Nothing

class (Applicative f) => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
    some :: f a -> f [a]
    some v = some_v where
         many_v = some_v <|> pure []
         some_v = (:) <$> v <*> many_v

    many :: f a -> f [a]
    many v = many_v where
         many_v = some_v <|> pure []
         some_v = (:) <$> v <*> many_v

instance Alternative Parser where
    empty = fail ""
    (Parser x) <|> (Parser y) = Parser $ \s ->
        case x s of
            Just x -> Just x
            Nothing -> y s


char :: Char -> Parser Char
char c = Parser charP 
   where charP [] = Nothing
         charP (x:xs) | x == c = Just (c,xs)
                      | otherwise = Nothing

--parseD = char 'd'
--test = runParser parseD "def"

parseD = char 'd' <|> char 'D'
--test = runParser parseD "Ddef"

string :: String -> Parser String
string = mapM char

space :: Parser Char
space = char ' ' <|> char '\n' <|> char '\r' <|> char '\t'

ss = many space
parseHW = (,) <$> (string "Hello" <* ss) <*> string "World"

testt = runParser parseHW "Hello World meee"


parseIntChar :: Parser Char
parseIntChar = char '0' <|> char '1' <|> char '2' <|> char '3' <|> char '4'
             <|> char '5' <|> char '6' <|> char '7' <|> char '8' <|> char '9'

parseInt :: Parser Int
parseInt = str2Int 0 <$> some parseIntChar

str2Int :: Int -> String -> Int
str2Int acc [] = acc
str2Int acc (x:xs) = str2Int (acc * 10 + (char2Int x)) xs

char2Int '0' = 0
char2Int '1' = 1
char2Int '2' = 2
char2Int '3' = 3
char2Int '4' = 4
char2Int '5' = 5
char2Int '6' = 6
char2Int '7' = 7
char2Int '8' = 8
char2Int '9' = 9

--main :: IO ()
--main = putStrLn "Hello, Haskell" >> putStrLn "hello worldddd"
--main = do
--    putStrLn "Hello, whats your name" >> getLine >>= \x -> putStrLn $ "hello, " ++ x