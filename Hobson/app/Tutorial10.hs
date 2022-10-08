{-# LANGUAGE FlexibleInstances, OverloadedStrings, ApplicativeDo #-}

module Tutorial10 where 
import Data.Attoparsec.Text hiding (take)
import Control.Applicative
import qualified Data.Text as T

type Ident = String
data Id a = Id a deriving Show

instance Functor Id where
  fmap f (Id x) = Id $ f x

instance Applicative Id where
   (Id f) <*> (Id x) = Id $ f x
   pure x = Id x

instance Monad Id where
   (Id x) >>= f = f x

instance MonadFail (Either String) where
   fail = Left

data State m a = St { runState :: (m -> (a,m)) }

instance Functor (State m) where
  fmap f (St x) = St $ \m -> let (a, m') = x m
                             in (f a, m')

instance Applicative (State m) where
  pure x = St $ \m -> (x,m) 
  (St f) <*> (St x) = St $ \m -> let (f',m') = f m in
                                 let (x', m'') = x m' in
                                   (f' x', m'')

instance Monad (State m) where
   (St xm) >>= f = St $ \m -> let (x,m') = xm m in
                              let (St g) = f x in
                                g m' 
get :: State m m
get = St $ \m -> (m, m)

set :: a -> State a () 
set x = St $ \m -> ((), x)
type Mem = [Value]

type M a = State Mem a

data Expr = Number Int 
          | Boolean Bool
          | Plus Expr Expr 
          | Minus Expr Expr 
          | If Expr Expr Expr
          | Var Ident 
          | Equals Expr Expr
          | Let Defn Expr 
          | Lam [Ident] Expr 
          | Apply Expr [Expr]
          | New
          | Deref Expr
          | Seq Expr Expr
          | Assign Expr Expr
          deriving (Show, Eq)

data Value = NumVal Int | BoolVal Bool | Closure [Ident] Expr Env | Null | MemAddr Int deriving (Show, Eq)

data Defn = Val Ident Expr
          | Rec Ident  Expr
          deriving (Show,Eq)

parseFun t = parseOnly parseExpr t

parseExpr :: Parser Expr
parseExpr = parsePM <|> parseExpr'

parseExpr' :: Parser Expr
parseExpr' = parseConst <|> parseIf <|> parseLet <|> parseLam <|> parseApp <|> parseVar

ss = skipSpace
atom = T.unpack <$> takeWhile1 (\c -> c /= ' '  && c /= '*' && c /= '-' && c /= ':')
parseConst ::  Parser Expr
parseConst = Number <$> decimal 
           <|> "true" *> pure (Boolean True)
           <|> "false" *> pure (Boolean False)

parseIf :: Parser Expr
parseIf = do
   "if" *> ss
   cond <- parseExpr <* ss
   "then" *> ss
   e1 <- parseExpr <* ss
   "else" *> ss
   e2 <- parseExpr <* ss
   return (If cond e1 e1)

parseVar = Var <$> atom

parseLet = do
   "Let" *> ss
   d <- parseDefn <* ss
   "in" *> ss
   e <- parseExpr
   return (Let d e)

parseDefn = Val <$> ("val" *> ss *> atom <* ss <* char '*' <* ss) <*> parseExpr
         <|> Rec <$> ("rec" *> ss *> atom <* ss <* char '*' <* ss) <*> parseExpr

parseLam = do
   char '!'
   atoms <- many1 (atom <* ss)
   "->" *> ss 
   expr <- parseExpr
   return $ Lam atoms expr
parsePM = parsePM' <|> parseTerm
parsePM' = do
   t <- parseTerm <* ss
   con <- ( char '+' *> pure Plus
          <|> char '-' *> pure Minus
          )
   ss
   e <- parseExpr
   return $ con t e

parseTerm = parseTerm' <|> parseFactor

parseTerm' = do
   f <- parseFactor <* ss
   "==" *> ss
   t <- parseTerm
   return $ Equals f t 

parseFactor = char '(' *> parseExpr <* char ')' <|> parseExpr'

parseApp = do 
   f <- "%{" *> parseExpr
   char ':' *> ss
   ins <- many (parseExpr <* ss)
   return $ Apply f ins 

type Env = [(Ident, Value)]

eval :: Expr -> Env -> M Value
eval (Number i) env = return $ NumVal i
eval (Boolean b) env = return $ BoolVal b 
eval (Equals e1 e2) env = BoolVal <$> ((==) <$> (eval e1 env) <*> (eval e2 env))
eval (Plus e1 e2) env = do { ~(NumVal n1) <- eval e1 env
                           ; ~(NumVal n2) <- eval e2 env
                           ; return $ NumVal (n1 + n2)
                           }

eval (Minus e1 e2) env = do { ~(NumVal n1) <- eval e1 env
                           ; ~(NumVal n2) <- eval e2 env
                           ; return $ NumVal (n1 - n2)
                           }

eval (Var i) env = return $ find env i
eval (Let d e) env = elab env d >>= eval e
eval (Lam ids e) env = return $ Closure ids e env
eval (If g e1 e2) env = eval g env >>= \r -> case r of
                       (BoolVal True) -> eval e1 env
                       (BoolVal False) -> eval e2 env
eval (Apply f xs) env = do { f' <- eval f env
                           ; xs' <- mapM (flip eval env) xs
                           ; apply f' xs'
                           }
eval (New) env = do { mem <- get
                      ; let ret = length mem
                      ; set $ mem ++ [Null] 
                      ; return $ MemAddr ret
                      }
eval (Deref e) env = do { ~(MemAddr i) <- eval e env
                        ; mem <- get
                        ; return $ mem !! i
                        }
eval (Seq e1 e2) env = eval e1 env >> eval e2 env

eval (Assign e1 e2) env = do { ~(MemAddr i) <- eval e1 env   
                             ; e2' <- eval e2 env
                             ; mem <- get
                             ; let mem' = take i mem ++ [e2'] ++ drop (i+1) mem
                             ; set mem'
                             ; return Null                          
                             }
apply :: Value -> [Value] -> M Value
apply (Closure ids e env) vals = eval e ((zip ids vals) ++ env)
apply _ _ = error "Using a value as if its a function"

find env i = snd $ head $ filter (\(i', _) -> i == i') env
elab env (Val i e) = eval e env >>= \e' -> return $ (i, e'):env
elab env (Rec i l@(Lam ids e)) = return env' where
       env' = (i, Closure ids e env'):env
elab _ _ = error "Only lambdas can be recursive"


e = Let ((Val "x") New) (Seq (Assign (Var "x") (Number 42)) (Assign (Var "x") (Plus (Deref (Var "x")) (Number 1))))
res = eval e []
test = runState res []
