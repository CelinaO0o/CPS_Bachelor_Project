module Interpreter where

-- Non-CPS Interpreter 
-- type Ident = String

-- data Expr = Const Int
--           | Var Ident
--           | Add Expr Expr
--           | Fun [Ident] Expr
--           | App Expr [Expr]
--           deriving Show

-- data Value = NumVal Int | FunVal [Ident] Expr Env
--     deriving (Show)

-- type Env = [(Ident, Value)]

-- eval :: Expr -> Env -> Value
-- eval (Const c) _ = NumVal c -- eval (Const 5) []
-- eval (Var v) env = snd $ head $ filter (\(i, _) -> i == v) env -- eval (Var "x") [("x", NumVal 3)]
-- eval (Add e1 e2) env = NumVal (int1+int2) where -- eval (Add (Var "x") (Const 5)) [("x", NumVal 3)]
--                         NumVal int1 = eval e1 env
--                         NumVal int2 = eval e2 env
-- eval (Fun args e) env = FunVal args e env
-- eval (App fun args) env = let FunVal ids e env' = eval fun env -- eval (App (Fun ["x", "y"] (Add (Var "x") (Var "y"))) [Const 1, Const 2]) []
--                               args' = map (`eval` env) args in
--                                 eval e (zip ids args' ++ env)
                          





-- CPS Interpreter 
type Ident = String 

data Expr = Const Int
          | Var Ident 
          | Add Expr Expr
          | Fun [Ident] Expr
          | App Expr [Expr]
          deriving Show

data Value = NumVal Int | FunVal [Ident] Expr Env | ArgVals [Value]
    deriving Show

type Env = [(Ident, Value)]

type Cont = Value -> Value

eval :: Expr -> Env -> Cont -> Value
eval (Const c) env k = k $ NumVal c
eval (Var v) env k = k $ snd $ head $ filter (\(i, _) -> i == v) env
eval (Add e1 e2) env k = eval e1 env (\(NumVal left) -> eval e2 env (\(NumVal right) -> k (NumVal(left+right))))
eval (Fun args e) env k = k $ FunVal args e env 
eval (App fun args) env k = eval fun env (\(FunVal params exp env') -> 
                            evalArgs args env [] (\(ArgVals argVals) -> 
                            eval exp (zip params argVals ++ env) k))

evalArgs ::[Expr] -> Env -> [Value] -> Cont -> Value
evalArgs (arg:args) env argVals k = eval arg env (\argVal -> evalArgs args env (argVal : argVals) k)
evalArgs [] _ argVals k = k (ArgVals (reverse argVals))


{-- CPS Interpreter Version 1.(Cont-Monad)

type Ident = String 

data Expr = Const Int
          | Var Ident 
          | Add Expr Expr
          deriving Show

data Value = NumVal Int
    deriving Show

type Env = [(Ident, Value)]

eval :: Expr -> Env -> ConT () Value Value
eval (Const c) env = ConT $ \k -> k NumVal c
eval (Var v) env = ConT $ \k -> k find v env
eval (Add e1 e2) env = ConT $ \k -> eval e1 env (\(NumVal left) -> eval e2 env (\(NumVal right) -> k (NumVal(left+right))))
find v env = snd $ head $ filter (\(i, _) -> i == v) env
--}