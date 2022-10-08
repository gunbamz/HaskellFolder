module Tutorial3 where 

data Error a = Error | Ok a deriving Show
safeDivide :: Error Int -> Errot Int -> Error Int
safeDivide (Error) _ = Error
safeDivide _ (Error) = Error
safeDivide (Ok a) (Ok b) = Ok (a `div` b)

test3 = safeDivide (Ok 5) (Ok 10)

data Person = Person { name :: String
                     , id   :: Int
                     , dob  :: (Int, Int, Int)
                     } 
--type :t name in repl