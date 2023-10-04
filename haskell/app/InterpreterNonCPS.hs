module InterpreterNonCPS where
import qualified Data.Map as Map
import Data.Maybe

-- Non-CPS Interpreter 
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

eval :: Expr -> Env -> Value
eval (Const c) _ = NumVal c -- eval (Const 5) []
eval (Var v) env = fromMaybe (error "Variable not found in environment" ) (lookup v env) -- eval (Var "x") [("x", NumVal 3)]
eval (Add expr1 expr2) env = NumVal (int1+int2) where -- eval (Add (Var "x") (Const 5)) [("x", NumVal 3)]
                        NumVal int1 = eval expr1 env
                        NumVal int2 = eval expr2 env
eval (Fun args expr) env = FunVal args expr env
eval (App fun args) env = let FunVal ids e env' = eval fun env -- eval (App (Fun ["x", "y"] (Add (Var "x") (Var "y"))) [Const 1, Const 2]) []
                              args' = map (`eval` env) args in
                                eval e (zip ids args' ++ env)
eval (Obj obj) env = ObjVal $ [(ident, eval expr env) | (ident, expr) <- obj]
eval (Field expr field) env =  case eval expr env of
  ObjVal fields -> fromMaybe (error "Field not found") (lookup field fields)
  _ -> error "Non-object value"
-- doesn't support obj saved as var in env:
-- eval (GetField obj field) env =  case eval (Obj obj) env of
--   ObjVal fields -> case Map.lookup field fields of
--     Just value -> value 