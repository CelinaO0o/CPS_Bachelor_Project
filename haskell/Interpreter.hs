module Interpreter where
import Data.Maybe ( fromMaybe )
import Control.Monad.Cont (ContT)

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
                          





-- CPS Interpreter, explicit Cont
type Ident = String 

data Expr = Const Int
          | Var Ident 
          | Add Expr Expr
          | Fun [Ident] Expr
          | App Expr [Expr]
          deriving Show

data Value = NumVal Int
           | VarVal Ident 
           | FunVal [Ident] Expr Env 
    deriving Show

type Env = [(Ident, Value)]

type Cont b a = (a -> b) -> b

eval :: Expr -> Env -> Cont Value Value
eval (Const c) env k = k $ NumVal c
eval (Var v) env k = k $ fromMaybe (VarVal v)  (lookup v env) -- what should be returned if v not in env?
eval (Add e1 e2) env k = eval e1 env (\(NumVal left) -> eval e2 env (\(NumVal right) -> k (NumVal(left+right))))
eval (Fun params exp) env k = k $ FunVal params exp env 
eval (App fun args) env k = eval fun env (\(FunVal params exp env') -> 
                            evalArgs args env (\argVals-> 
                            eval exp (zip params argVals ++ env') k))


evalArgs :: [Expr] -> Env -> Cont Value [Value]
evalArgs [] _ k = k []
evalArgs (arg : args) env k = eval arg env (\argValue -> evalArgs args env (\restArgs -> k (argValue : restArgs)))


 
-- CPS Interpreter, monadic

-- type Ident = String 

-- data Expr = Const Int
--           | Var Ident 
--           | Add Expr Expr
--           | Fun [Ident] Expr
--           | App Expr [Expr]
--           deriving Show

-- data Value = NumVal Int
--            | VarVal Ident 
--            | FunVal [Ident] Expr Env 
--     deriving Show

-- type Env = [(Ident, Value)]


-- eval :: Expr -> Env -> ContT Value Value
-- eval (Const c) env k = k $ NumVal c
-- eval (Var v) env k = k $ fromMaybe (VarVal v)  (lookup v env) -- what should be returned if v not in env?
-- eval (Add e1 e2) env k = eval e1 env (\(NumVal left) -> eval e2 env (\(NumVal right) -> k (NumVal(left+right))))
-- eval (Fun params exp) env k = k $ FunVal params exp env 
-- eval (App fun args) env k = eval fun env (\(FunVal params exp env') -> 
--                             evalArgs args env (\argVals-> 
--                             eval exp (zip params argVals ++ env') k))


-- evalArgs :: [Expr] -> Env -> ContT Value [Value]
-- evalArgs [] _ k = k []
-- evalArgs (arg : args) env k = eval arg env (\argValue -> evalArgs args env (\restArgs -> k (argValue : restArgs)))
