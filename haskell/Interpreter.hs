module Interpreter where
import Data.Maybe ( fromMaybe )
import Control.Monad.Cont (ContT)

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
eval (Var v) env k = k $ fromMaybe (VarVal v)  (lookup v env) 
eval (Add e1 e2) env k = eval e1 env (\(NumVal left) -> eval e2 env (\(NumVal right) -> k (NumVal(left+right))))
eval (Fun params exp) env k = k $ FunVal params exp env 
eval (App fun args) env k = eval fun env (\(FunVal params exp env') -> 
                            evalArgs args env (\argVals-> 
                            eval exp (zip params argVals ++ env') k))


evalArgs :: [Expr] -> Env -> Cont Value [Value]
evalArgs [] _ k = k []
evalArgs (arg : args) env k = eval arg env (\argValue -> evalArgs args env (\restArgs -> k (argValue : restArgs)))

