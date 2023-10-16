module InterpreterNonCPS where
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Trans.State

-- Non-CPS Interpreter 
type Ident = String

data Expr = Const Int
          | Var Ident
          | Add Expr Expr
          | Fun [Ident] Expr
          | App Expr [Expr]
          | Obj [(Ident, Expr)]
          | Field Expr Ident
  deriving (Show, Eq)

data Value = NumVal Int
           | FunVal [Ident] Expr Env 
           | ObjVal [(Ident, Value)]
  deriving (Show, Eq)

type Env = [(Ident, Value)]


eval :: Expr -> Env -> Value
eval (Const c) _ = NumVal c 
eval (Var v) env = fromMaybe (error "Variable not found in environment" ) (lookup v env) 
eval (Add expr1 expr2) env = NumVal (int1+int2) where 
                        NumVal int1 = eval expr1 env
                        NumVal int2 = eval expr2 env
eval (Fun args expr) env = FunVal args expr env
eval (App fun args) env = let FunVal ids e env' = eval fun env 
                              args' = map (`eval` env) args in
                                eval e (zip ids args' ++ env)
eval (Obj obj) env = ObjVal $ [(ident, eval expr env) | (ident, expr) <- obj]
eval (Field expr field) env =  case eval expr env of
  ObjVal fields -> fromMaybe (error "Field not found") (lookup field fields)
  _ -> error "Non-object value"