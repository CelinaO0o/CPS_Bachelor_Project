module InterpreterMonadic where
import Data.Maybe ( fromMaybe )
import Control.Monad.Cont (ContT)

-- CPS Interpreter, monadic

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


eval :: Expr -> Env -> ContT Value Maybe Value
eval (Const c) env k = k $ NumVal c
eval (Var v) env k = k $ fromMaybe (VarVal v)  (lookup v env) -- what should be returned if v not in env?
eval (Add e1 e2) env k = eval e1 env (\(NumVal left) -> eval e2 env (\(NumVal right) -> k (NumVal(left+right))))
eval (Fun params exp) env k = k $ FunVal params exp env 
eval (App fun args) env k = eval fun env (\(FunVal params exp env') -> 
                            evalArgs args env (\argVals-> 
                            eval exp (zip params argVals ++ env') k))


evalArgs :: [Expr] -> Env -> ContT Value Maybe [Value]
evalArgs [] _ k = k []
evalArgs (arg : args) env k = eval arg env (\argValue -> evalArgs args env (\restArgs -> k (argValue : restArgs)))
