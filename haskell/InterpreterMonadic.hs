module InterpreterMonadic where
import Data.Maybe ( fromMaybe )
import Control.Monad.Cont (ContT (runContT))
import Control.Monad.Reader (Reader)

-- CPS Interpreter, monadic

type Ident = String

data Expr = Const Int
          | Var Ident
          | Add Expr Expr
          | Fun [Ident] Expr
          | App Expr [Expr]
          deriving Show

data Value = NumVal Int
           | FunVal [Ident] Expr Env
    deriving Show

type Env = [(Ident, Value)]


eval :: Expr -> Env -> ContT Value Maybe Value
eval (Const c) env = return $ NumVal c
eval (Var v) env = return $ fromMaybe (error "Field not found") (lookup v env)
eval (Add e1 e2) env = eval e1 env >>= (\(NumVal left) -> eval e2 env >>= (\(NumVal right) -> return (NumVal(left+right))))
eval (Fun params exp) env = return $ FunVal params exp env 
eval (App fun args) env = eval fun env >>= (\(FunVal params exp env') -> 
                            evalArgs args env >>= (\argVals-> 
                            eval exp (zip params argVals ++ env')))


evalArgs :: [Expr] -> Env -> ContT Value Maybe [Value]
evalArgs [] _ = return []
evalArgs (arg : args) env = eval arg env >>= (\argValue -> evalArgs args env >>= (\restArgs -> return (argValue : restArgs)))


--TODO: reader-monad
-- eval :: Expr -> Env -> ContT Value (Reader Env) Value
-- eval (Const c) env = return $ NumVal c
-- eval (Var v) env = return $ fromMaybe (error "Variable not found in environment") (lookup v env)
-- eval (Add e1 e2) env = eval e1 env >>= (\(NumVal left) -> eval e2 env >>= (\(NumVal right) -> return (NumVal(left+right))))
-- eval (Fun params exp) env = return $ FunVal params exp env 
-- eval (App fun args) env = eval fun env >>= (\(FunVal params exp env') -> 
--                             evalArgs args env >>= (\argVals-> 
--                             eval exp (zip params argVals ++ env')))


-- evalArgs :: [Expr] -> Env -> ContT Value Maybe [Value]
-- evalArgs [] _ = return []
-- evalArgs (arg : args) env = eval arg env >>= (\argValue -> evalArgs args env >>= (\restArgs -> return (argValue : restArgs)))
