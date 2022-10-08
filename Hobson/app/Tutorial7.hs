module Tutorial7 where 

type Ident = String

data Expr = Number Int | Plus Expr Expr | Minus Expr Expr | Var Ident | Let Ident Expr Expr deriving Show

data Value = NumVal Int deriving Show

type Env = [(Ident, Value)]

eval :: Expr -> Env -> Value
eval (Number i) env = NumVal i
eval (Plus e1 e2) env = let (NumVal n1) = eval e1 env in
                        let (NumVal n2) = eval e2 env in
                        NumVal (n1 + n2)

eval (Minus e1 e2) env = let (NumVal n1) = eval e1 env in
                         let (NumVal n2) = eval e1 env in
                         NumVal (n2 * n1)

eval (Var i) env = find env i
eval (Let i e1 e2) env = eval e2 (elab env i e1)
find env i = snd $ head $ filter (\(i', _) -> i == i') env
elab env i e = (i, eval e env):env

--test
--expr = Minus (Number 20) (Number 12)
expr = Let "x" (Number 10) (Minus (Number 22) (Var "x"))
test = eval expr []
