module Tutorial8b where 

type Ident = String

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
          deriving (Show, Eq)

data Value = NumVal Int | BoolVal Bool | Closure [Ident] Expr Env deriving (Show, Eq)

data Defn = Val Ident Expr
          | Rec Ident  Expr
          deriving (Show,Eq)

type Env = [(Ident, Value)]

eval :: Expr -> Env -> Value
eval (Number i) env = NumVal i
eval (Boolean b) env = BoolVal b 
eval (Equals e1 e2) env = BoolVal $ (eval e1 env) == (eval e2 env)
eval (Plus e1 e2) env = let (NumVal n1) = eval e1 env in
                        let (NumVal n2) = eval e2 env in
                        NumVal (n1 + n2)

eval (Minus e1 e2) env = let (NumVal n1) = eval e1 env in
                         let (NumVal n2) = eval e1 env in
                         NumVal (n1 - n2)

eval (Var i) env = find env i
eval (Let d e) env = eval e (elab env d)
eval (Lam ids e) env = Closure ids e env
eval (If g e1 e2) env = case eval g env of
                       (BoolVal True) -> eval e1 env
                       (BoolVal False) -> eval e2 env
--eval (Let i e1 e2) env = eval e2 (elab env i e1)
eval (Apply f xs) env = apply f' xs'
     where f' = eval f env
           xs' = map (flip eval env) xs

apply :: Value -> [Value] -> Value
apply (Closure ids e env) vals = eval e (zip ids vals ++ env)
apply _ _ = error "Using a value as if its a function"

find env i = snd $ head $ filter (\(i', _) -> i == i') env
elab env (Val i e) = (i, eval e env):env
elab env (Rec i (Lam args e)) = env' where
       env' = (i, Closure args e env'):env
elab _ _ = error "Only lambdas can be recursive"

--test
--expr = Minus (Number 20) (Number 12)
--expr = Let (Val "x" $ Number 100) (Var "x")
--test = eval expr []

--e = Let (Val "add" (Lam ["x", "y"] (Plus (Var "x") (Var "y")))) (Apply (Var "add") [Number 1, Number 2])
--test = eval e []
e = Let (Rec "sum" (Lam ["n"] (If (Equals (Var "n") (Number 0)) (Number 0) (Plus (Var "n") (Apply (Var "sum") [Minus (Var "n") (Number 1)])))))
ex = e (Apply (Var "sum") [Number 10])
test = eval ex []
