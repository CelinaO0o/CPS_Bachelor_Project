module InterpreterMonadic where
import Data.Maybe ( fromMaybe )
import Control.Monad
import Control.Monad.Trans.Cont (ContT)

-- CPS Interpreter, monadic

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


eval :: Expr -> Env -> ContT Value Maybe Value
eval (Const c) env = return $ NumVal c
eval (Var v) env = return $ fromMaybe (error "Field not found") (lookup v env)
eval (Add expr1 expr2) env = do
    (NumVal left) <- eval expr1 env
    (NumVal right) <- eval expr2 env
    return (NumVal (left+right))
eval (Fun params exp) env = return $ FunVal params exp env
eval (App fun args) env = do
    (FunVal params expr env') <- eval fun env
    argVals <- evalArgs args env
    eval expr (zip params argVals ++ env')
eval (Obj obj) env = do
    objVals <- evalFields obj env
    return (ObjVal objVals)
-- why does eval (Obj obj) env = return (ObjVal (evalFields obj env)) not work?
eval (Field expr field) env = do
    fieldVal <- eval expr env
    case fieldVal of
        ObjVal fields -> return $ fromMaybe (error "Field not found") (lookup field fields)
        _ -> return $ error "Non-object value"
        
evalArgs :: [Expr] -> Env -> ContT Value Maybe [Value]
evalArgs args env = mapM (`eval` env) args

evalFields :: [(Ident, Expr)] -> Env -> ContT Value Maybe [(Ident, Value)]
evalFields fields env = zipWithM (\(ident, expr) _ -> do
                                      exprVal <- eval expr env
                                      return (ident, exprVal))
                                   fields [0..]


