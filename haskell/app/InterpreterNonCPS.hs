module InterpreterNonCPS where
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Trans.State hiding (State)

-- Non-CPS Interpreter 

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


-- Implementation of State (incomplete)

-- type Ident = String

-- type Address = Int

-- data Expr = Const Int                -- a constant integer
--           | Var Ident                -- a string variable
--           | Add Expr Expr            -- Addition of two expressions
--           | Fun [Ident] Expr         -- Function [parameters] functionbody
--           | App Expr [Expr]          -- Application function [argumentvalues] 
--           | Obj (Map.Map Ident Expr) -- Object Map{fieldname -> value}
--           | Field Expr Ident         -- Field object fieldname
--           | SetField Expr Ident Expr -- SetField object fieldname new_value TODO nicht in Expr?
--   deriving (Show, Eq)

-- data Value = NumVal Int              -- Numeric Value
--            | FunVal [Ident] Expr Env -- Function_value [parameters] functionbody environment
--            | PtrVal Address          -- Pointer_value
--   deriving (Show, Eq)

-- newtype ObjValue = ObjVal (Map.Map Ident Value) -- Object_value Map{fieldname -> value}
--   deriving (Show, Eq)

-- type Env = (Map.Map Ident Value) -- Map{variable -> value}

-- type Result = (Value, State)

-- eval :: Expr -> Env -> State -> Result
-- eval (Const c) _ s = (NumVal c, s)
-- eval (Var v) env s = (env Map.! v, s)
-- eval (Add expr1 expr2) env s = (NumVal (int1+int2), s) where
--                         (NumVal int1, s1) = eval expr1 env s
--                         (NumVal int2, s2) = eval expr2 env s
-- eval (Fun params expr) env s = (FunVal params expr env, s)
-- eval (App fun args) env s = let (FunVal params expr env', s') = eval fun env s
--                                 args' = map (\arg -> fst $ eval arg env s) args
--                                 newEnv = Map.union env (Map.fromList (zip params args')) in
--                                 eval expr newEnv s'
-- eval (Obj obj) env s = 