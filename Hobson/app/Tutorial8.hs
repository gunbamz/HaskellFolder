module Tutorial8 where 

type Ident = String

data Expr = Number Int 
          | Plus Expr Expr 
          | Minus Expr Expr 
          | Var Ident 
          | Let Ident Expr Expr 
          | Lam [Ident] Expr 
          | Apply Expr [Expr]
          deriving Show

data Value = NumVal Int | Closure [Ident] Expr Env deriving Show

type Env = [(Ident, Value)]

eval :: Expr -> Env -> Value
eval (Number i) env = NumVal i
eval (Plus e1 e2) env = let (NumVal n1) = eval e1 env in
                        let (NumVal n2) = eval e2 env in
                        NumVal (n1 + n2)

eval (Minus e1 e2) env = let (NumVal n1) = eval e1 env in
                         let (NumVal n2) = eval e1 env in
                         NumVal (n1 - n2)

eval (Var i) env = find env i
eval (Let i e1 e2) env = eval e2 (elab env i e1)
eval (Lam ids e) env = Closure ids e env
eval (Apply f xs) env = apply f' xs'
     where f' = eval f env
           xs' = map (flip eval env) xs

apply :: Value -> [Value] -> Value
apply (Closure ids e env) vals = eval e (zip ids vals ++ env)
apply _ _ = error "Using a value as if its a function"

find env i = snd $ head $ filter (\(i', _) -> i == i') env
elab env i e = (i, eval e env):env

--test
expr = Minus (Number 20) (Number 12)
--expr = Let "x" (Number 100) (Plus (Number 22) (Var "x"))
test = eval expr []

e = Let "Add" (Lam ["x", "y"]  (Plus (Var "x") (Var "y"))) (Apply (Var "Add") [Number 1,  Number 2])
test2 = eval e []
