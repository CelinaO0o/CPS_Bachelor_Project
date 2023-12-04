module InterpreterCPS where
import Data.Maybe ( fromMaybe )
import qualified Data.Map as Map

-- CPS Interpreter, explicit

type Ident = String

type Address = Int

data Expr = Const Int                -- a constant integer
          | Var Ident                -- a string variable
          | Add Expr Expr            -- Addition of two expressions
          | Fun [Ident] Expr         -- Function [parameters] functionbody
          | App Expr [Expr]          -- Application function [argumentvalues] 
          | Obj (Map.Map Ident Expr) -- Object Map{fieldname -> value}
          | Field Expr Ident         -- Field object fieldname
          | SetField Expr Ident Expr -- SetField object fieldname new_value TODO nicht in Expr?
  deriving (Show, Eq)

data Value = NumVal Int              -- Numeric Value
           | FunVal [Ident] Expr Env -- Function_value [parameters] functionbody environment
           | PtrVal Address          -- Pointer_value
  deriving (Show, Eq)

newtype ObjValue = ObjVal (Map.Map Ident Value) -- Object_value Map{fieldname -> value}
  deriving (Show, Eq)

type Env = (Map.Map Ident Value) -- Map{variable -> value}

data State = State { free :: Address, store :: Map.Map Address ObjValue}
  deriving (Show, Eq)
 
type Result = (Value, State)

type Cont a = a -> State -> Result

eval :: Expr -> Env -> State -> Cont Value -> Result
eval (Const c) env s k = k (NumVal c) s
eval (Var v) env s k = k (env Map.! v) s
eval (Add expr1 expr2) env s k = eval expr1 env s (\(NumVal left) s -> eval expr2 env s (\(NumVal right) s -> k (NumVal (left+right)) s))
eval (Fun params expr) env s k = k (FunVal params expr env) s
eval (App fun args) env s k = eval fun env s (\(FunVal params expr env') s ->
  evalMultiple args env s (\argVals s ->
  eval expr (Map.union env' (Map.fromList (zip params argVals))) s k))
eval (Obj obj) env s k = evalMultiple (Map.elems obj) env s (\fieldVals s ->
  let newaddr = free s + 1 in
    let newObjVal = ObjVal (Map.fromList(zip (Map.keys obj) fieldVals)) in
      let s' = State {free = newaddr, store = Map.insert (free s) newObjVal (store s)} in
        k (PtrVal (free s)) s')
eval (Field obj field) env s k = case eval obj env s k of
  (PtrVal ptr, State free store) -> let ObjVal valAtPtr = store Map.! ptr in
    k (valAtPtr Map.! field) (State free store)
  _ -> k (error "Non-object value") s
eval (SetField obj field expr) env s k = case eval obj env s k of
  (PtrVal address, State f s) ->
    let ObjVal objVal = s Map.! address in
    let fieldVal = fst $ eval expr env (State f s) k in
    let objVal' = ObjVal $ Map.insert field fieldVal objVal in
    let s' = State {free = f, store = Map.insert address objVal' s} in 
      k fieldVal s' 

evalMultiple :: [Expr] -> Env -> State -> Cont [Value] -> Result
evalMultiple [] env s k = k [] s
evalMultiple (arg : args) env s k = eval arg env s (\argVal s -> evalMultiple args env s (\restVals s -> k (argVal : restVals) s))
