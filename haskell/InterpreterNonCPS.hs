module InterpreterNonCPS where
import qualified Data.Map as Map

-- Non-CPS Interpreter 
type Ident = String

type Object = (Map Ident Expr)
type ObjValue = (Map Ident Value)

data Expr = Const Int
          | Var Ident
          | Add Expr Expr
          | Fun [Ident] Expr
          | App Expr [Expr]
          | Obj Object
  deriving Show

data Value = NumVal Int 
           | FunVal [Ident] Expr Env
           | ObjVal ObjValue
  deriving (Show)

type Env = [(Ident, Value)]

eval :: Expr -> Env -> Value
eval (Const c) _ = NumVal c -- eval (Const 5) []
eval (Var v) env = snd $ head $ filter (\(i, _) -> i == v) env -- eval (Var "x") [("x", NumVal 3)]
eval (Add e1 e2) env = NumVal (int1+int2) where -- eval (Add (Var "x") (Const 5)) [("x", NumVal 3)]
                        NumVal int1 = eval e1 env
                        NumVal int2 = eval e2 env
eval (Fun args e) env = FunVal args e env
eval (App fun args) env = let FunVal ids e env' = eval fun env -- eval (App (Fun ["x", "y"] (Add (Var "x") (Var "y"))) [Const 1, Const 2]) []
                              args' = map (`eval` env) args in
                                eval e (zip ids args' ++ env)
eval (Obj o) env = ObjVal $ Map.fromList [(ident, eval expr env) | (ident, expr) <- Map.toList o]