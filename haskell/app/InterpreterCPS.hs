module InterpreterCPS where
import Data.Maybe ( fromMaybe )
import qualified Data.Map as Map

-- CPS Interpreter, explicit Cont

type Ident = String

type Address = Int

data Expr = Const Int
          | Var Ident
          | Add Expr Expr
          | Fun [Ident] Expr
          | App Expr [Expr]
          | Obj (Map.Map Ident Expr)
          | Field Expr Ident
          | SetField Address Ident Expr
  deriving (Show, Eq)

data Value = NumVal Int
           | FunVal [Ident] Expr Env
           | PtrVal Address
           | ObjVal (Map.Map Ident Value)
  deriving (Show, Eq)

type Env = (Map.Map Ident Value) -- (Map.Map Ident Result) ?

data State = State { free :: Address, store :: Map.Map Address Value }
  deriving (Show, Eq)

type Result = (Value, State)

type Cont a = a -> State -> Result

eval :: Expr -> Env -> State -> Cont Value -> Result
eval (Const c) env s k = k (NumVal c) s
eval (Var v) env s k = k (env Map.! v) s
eval (Add expr1 expr2) env s k = eval expr1 env s (\(NumVal left) s -> eval expr2 env s (\(NumVal right) s -> k (NumVal (left+right)) s))
eval (Fun params expr) env s k = k (FunVal params expr env) s
eval (App fun args) env s k = eval fun env s (\(FunVal params expr env') s -> -- will this work for k /= sid ? 
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
eval (SetField address field expr) env (State f s) k = 
  let ObjVal objVal = s Map.! address in
  let fieldVal = fst $ eval expr env (State f s) k in
  let objVal' = ObjVal $ Map.insert field fieldVal objVal in
  let s' = State {free = f, store = Map.insert address objVal' s} in 
    k (PtrVal address) s' -- what should be the output?


evalMultiple :: [Expr] -> Env -> State -> Cont [Value] -> Result
evalMultiple [] env s k = k [] s
evalMultiple (arg : args) env s k = eval arg env s (\argVal s -> evalMultiple args env s (\restVals s -> k (argVal : restVals) s))

getAdress :: Value -> State -> [Address]
getAdress objVal (State f s) = Map.keys $ Map.filter (== objVal) s