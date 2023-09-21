module Interpreter where
import Data.Maybe ( fromMaybe )
import Control.Monad.Cont (ContT)
import qualified Data.Map as Map

-- CPS Interpreter, explicit Cont
type Ident = String

type Object = (Map.Map Ident Expr)
type ObjValue = (Map.Map Ident Value)

data Expr = Const Int
          | Var Ident
          | Add Expr Expr
          | Fun [Ident] Expr
          | App Expr [Expr]
          | Obj Object
          | GetField Expr Ident -- GetField Object Ident
  deriving Show

data Value = NumVal Int
           | FunVal [Ident] Expr Env 
           | ObjVal ObjValue
  deriving (Show)

type Env = [(Ident, Value)]

type Cont b a = (a -> b) -> b

eval :: Expr -> Env -> Cont Value Value
eval (Const c) env k = k $ NumVal c
eval (Var v) env k = k $ fromMaybe (error "Variable not found in environment")  (lookup v env) 
eval (Add e1 e2) env k = eval e1 env (\(NumVal left) -> eval e2 env (\(NumVal right) -> k (NumVal(left+right))))
eval (Fun params e) env k = k $ FunVal params e env 
eval (App fun args) env k = eval fun env (\(FunVal params e env') -> 
                            evalArgs args env (\argVals-> 
                            eval e (zip params argVals ++ env') k))
-- TODO
-- eval (Obj obj) env k = ObjVal $ Map.fromList [(ident, eval expr env) | (ident, expr) <- Map.toList obj]
-- eval (GetField e field) env k =  case eval e env of
--   ObjVal fields -> case Map.lookup field fields of
--     Just value -> value
--     Nothing -> error "Field not found"
--   _ -> error "non-object value"


evalArgs :: [Expr] -> Env -> Cont Value [Value]
evalArgs [] _ k = k []
evalArgs (arg : args) env k = eval arg env (\argValue -> evalArgs args env (\restArgs -> k (argValue : restArgs)))

