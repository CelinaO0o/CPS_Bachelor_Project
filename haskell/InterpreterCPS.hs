module InterpreterCPS where
import Data.Maybe ( fromMaybe )
import Control.Monad.Cont (ContT)
import qualified Data.Map as Map

-- CPS Interpreter, explicit Cont
type Ident = String

data Expr = Const Int
          | Var Ident
          | Add Expr Expr
          | Fun [Ident] Expr
          | App Expr [Expr]
          | Obj [(Ident, Expr)]
          | Field Expr Ident
  deriving Show

data Value = NumVal Int
           | FunVal [Ident] Expr Env
           | ObjVal [(Ident, Value)]
  deriving (Show)

type Env = [(Ident, Value)]

type Cont b a = (a -> b) -> b

eval :: Expr -> Env -> Cont Value Value
eval (Const c) env k = k $ NumVal c
eval (Var v) env k = k $ fromMaybe (error "Variable not found in environment")  (lookup v env)
eval (Add expr1 expr2) env k = eval expr1 env (\(NumVal left) -> eval expr2 env (\(NumVal right) -> k (NumVal (left+right))))
eval (Fun params expr) env k = k $ FunVal params expr env
eval (App fun args) env k = eval fun env (\(FunVal params expr env') ->
                            evalArgs args env (\argVals->
                            eval expr (zip params argVals ++ env') k))
eval (Obj obj) env k = evalFields obj env (k . ObjVal) -- \objVal -> k $ ObjVal objVal
eval (Field expr field) env k =  case eval expr env k of
  ObjVal fields -> k $ fromMaybe (error "Field not found") (lookup field fields)
  _ -> k $ error "Non-object value"

evalArgs :: [Expr] -> Env -> Cont Value [Value]
evalArgs [] _ k = k []
evalArgs (arg : args) env k = eval arg env (\argValue -> evalArgs args env (\restArgs -> k (argValue : restArgs)))

evalFields :: [(Ident, Expr)] -> Env -> Cont Value [(Ident, Value)]
evalFields [] _ k = k []
evalFields ((ident, expr):xs) env k = eval (Field expr ident) env (\exprVal -> evalFields xs env (\restFields -> k ((ident, exprVal) : restFields)))
