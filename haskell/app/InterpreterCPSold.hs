module InterpreterCPSold where
import Data.Maybe ( fromMaybe )
import qualified Data.Map as Map
import Test.QuickCheck hiding (Fun)

-- CPS Interpreter, explicit Cont

type Ident = String
data Expr = Const Int                -- a constant integer
          | Var Ident                -- a string variable
          | Add Expr Expr            -- Addition of two expressions
          | Fun [Ident] Expr         -- Function [parameters] functionbody
          | App Expr [Expr]          -- Application function [argumentvalues] 
          | Obj [(Ident, Expr)]      -- Object [(fieldname, value)]
          | Field Expr Ident         -- Field object fieldname
  deriving (Show, Eq)

data Value = NumVal Int              -- Numeric Value
           | FunVal [Ident] Expr Env -- Function_value [parameters] functionbody environment
           | ObjVal [(Ident, Value)] -- Object_value [(fieldname, value)]
  deriving (Show, Eq)

type Env = [(Ident, Value)]          -- [(variable, value)]

type Cont b a = (a -> b) -> b

eval :: Expr -> Env -> Cont Value Value
eval (Const c) env k = k $ NumVal c
eval (Var v) env k = k $ fromMaybe (error "Variable not found in environment")  (lookup v env)
eval (Add expr1 expr2) env k = eval expr1 env (\(NumVal left) -> eval expr2 env (\(NumVal right) -> k (NumVal (left+right))))
eval (Fun params expr) env k = k $ FunVal params expr env
eval (App fun args) env k = eval fun env (\(FunVal params expr env') ->
                            evalArgs args env (\argVals->
                            eval expr (zip params argVals ++ env') k))

evalArgs :: [Expr] -> Env -> Cont Value [Value]
evalArgs args env k = k $ map (\arg -> eval arg env id) args


testExplicit :: IO () 
testExplicit = do
    putStrLn "\n-------- Constant evaluation: ----------------------------"
    let prop_const0 x = eval (Const x) [] id == NumVal x
    quickCheck prop_const0
    let prop_const1 x z = eval (Const x) [("z", NumVal z)] id == NumVal x
    quickCheck prop_const1

    putStrLn "\n-------- Variable evaluation: ----------------------------"
    let prop_var x y = eval (Var "x") [("x", NumVal x), ("y", NumVal y)] id == NumVal x
    quickCheck prop_var

    putStrLn "\n-------- Addition evaluation: ----------------------------"
    let prop_add0 x y = eval (Add (Const x) (Const y)) [("z", NumVal 4)] id == NumVal (x+y)
    quickCheck prop_add0
    let prop_add1 x y = eval (Add (Const x) (Var "y")) [("z", NumVal 5), ("y", NumVal y)] id == NumVal (x+y)
    quickCheck prop_add1
    let prop_add2 x y = eval (Add (Var "x") (Var "y")) [("x", NumVal x), ("y", NumVal y)] id == NumVal (x+y)
    quickCheck prop_add2

    putStrLn "\n-------- Function evaluation: ----------------------------"
    let f = Fun ["x", "y"] (Add (Var "x") (Var "y"))
    let prop_fun0 x y = eval f [("x", NumVal x), ("y", NumVal y)] id == FunVal ["x", "y"] (Add (Var "x") (Var "y")) [("x", NumVal x), ("y", NumVal y)]
    quickCheck prop_fun0

    putStrLn "\n-------- Function application evaluation: ----------------"
    let prop_funapp0 x y = eval (App f [Var "x", Var "y"]) [("x", NumVal x), ("y", NumVal y)] id == eval (Add (Const x) (Const y)) [] id 
    quickCheck prop_funapp0
