module Tutorial6 where 

--class (Semigroup m) => Monoid m where
--    mempty :: m
--    mappend :: m -> m -> m
--    mappend = (<>)

--class Functor f where  
--    fmap :: (a -> b) -> (f a -> f b)

data Failure a = Fail | Ok a deriving (Show)

instance Functor Failure where 
    fmap f (Ok x) = Ok $ f x
    fmap f (Fail) = Fail

--class (Functor f) => Applicative f where
--    pure :: a -> f a 
--    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Failure where 
    pure = Ok
    (Ok f) <*> (Ok x) = (Ok $ f x)
    (Fail) <*> _ = Fail
    _ <*> (Fail) = Fail

data Person = Person String Int Int deriving (Show)

test = Person <$> (Ok "james") <*> (Ok 21) <*> (Ok 10)

--class (Applicative m) => Monad m where
--    return :: a-> m Applicative
--    return = pure
--    (>>=) :: m a -> (a -> m b) -> m b
--return x >>= f = return $ f x

--f x >>= return = f x
--(f >>= g) >>= h = f >>= (\x -> g x >>= h)

instance Monad Failure where
    (Ok x) >>= f = f x
    (Fail) >>= f = Fail

safeDiv :: Failure Int -> Failure Int -> Failure Int
safeDiv xm ym = xm >>= (\x ->
                ym >>= (\y -> 
                  if y == 0 then Fail
                            else  return (x `div` y))) 
-- with do notation
--safeDiv xm ym = do
            -- x <- xm
            -- y <- ym
            -- if (y == 0) then Fail
                         --else return (x `div` y)

testt = safeDiv (Ok 10) (Ok 2)