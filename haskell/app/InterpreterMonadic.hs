module InterpreterMonadic where
import Data.Maybe ( fromMaybe )
import Control.Monad
import Control.Monad.Trans.Cont (ContT)

-- CPS Interpreter, monadic

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


