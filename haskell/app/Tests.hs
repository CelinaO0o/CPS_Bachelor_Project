module Tests where

import InterpreterNonCPS as NonCPS
import InterpreterCPS as CPS
import InterpreterMonadic as Monadic
import qualified Data.Map as Map
import Data.Maybe
import Test.QuickCheck
import Control.Monad.Trans.Cont

testAll :: IO ()
testAll = do 
    putStrLn "\n-------- Non CPS Interpreter: ------------------------------------------"
    testNonCPS
    putStrLn "\n-------- Explicit CPS Interpreter: -------------------------------------"
    testExplicit
    putStrLn "\n-------- Monadic CPS Interpreter: --------------------------------------"
    testMonadic


testNonCPS :: IO ()
testNonCPS = do

    -- let eval = NonCPS.eval 
    --     Const = NonCPS.Const -- won't work TODO
    
    putStrLn "\n-------- Constant evaluation: ----------------------------"
    let prop_const0 x = NonCPS.eval (NonCPS.Const x) [] == NonCPS.NumVal x
    quickCheck prop_const0
    let prop_const1 x z = NonCPS.eval (NonCPS.Const x) [("z", NonCPS.NumVal z)] == NonCPS.NumVal x
    quickCheck prop_const1
    
    putStrLn "\n-------- Variable evaluation: ----------------------------"
    let prop_var x y = NonCPS.eval (NonCPS.Var "x") [("x", NonCPS.NumVal x), ("y", NonCPS.NumVal y)] == NonCPS.NumVal x
    quickCheck prop_var

    putStrLn "\n-------- Addition evaluation: ----------------------------"
    let prop_add0 x y = NonCPS.eval (NonCPS.Add (NonCPS.Const x) (NonCPS.Const y)) [("z", NonCPS.NumVal 4)] == NonCPS.NumVal (x+y)
    quickCheck prop_add0
    let prop_add1 x y = NonCPS.eval (NonCPS.Add (NonCPS.Const x) (NonCPS.Var "y")) [("z", NonCPS.NumVal 5), ("y", NonCPS.NumVal y)] == NonCPS.NumVal (x+y)
    quickCheck prop_add1
    let prop_add2 x y = NonCPS.eval (NonCPS.Add (NonCPS.Var "x") (NonCPS.Var "y")) [("x", NonCPS.NumVal x), ("y", NonCPS.NumVal y)] == NonCPS.NumVal (x+y)
    quickCheck prop_add2

    putStrLn "\n-------- Function evaluation: ----------------------------"
    let f = NonCPS.Fun ["x", "y"] (NonCPS.Add (NonCPS.Var "x") (NonCPS.Var "y"))
    let prop_fun0 x y = NonCPS.eval f [("x", NonCPS.NumVal x), ("y", NonCPS.NumVal y)] == NonCPS.FunVal ["x", "y"] (NonCPS.Add (NonCPS.Var "x") (NonCPS.Var "y")) [("x", NonCPS.NumVal x), ("y", NonCPS.NumVal y)]
    quickCheck prop_fun0

    putStrLn "\n-------- Function application evaluation: ----------------"
    let prop_funapp0 x y = NonCPS.eval (NonCPS.App f [NonCPS.Var "x", NonCPS.Var "y"]) [("x", NonCPS.NumVal x), ("y", NonCPS.NumVal y)] == NonCPS.eval (NonCPS.Add (NonCPS.Const x) (NonCPS.Const y)) []
    quickCheck prop_funapp0
    
    putStrLn "\n-------- Object evaluation: ------------------------------"
    let obj0 = NonCPS.Obj [("field0", NonCPS.Const 42), ("field1", NonCPS.Const 99)]
    let prop_obj0 x = NonCPS.eval obj0 [("x", NonCPS.NumVal x)] == NonCPS.ObjVal [("field0", NonCPS.NumVal 42), ("field1", NonCPS.NumVal 99)]
    quickCheck prop_obj0
    let obj1 = NonCPS.Obj [("field0", NonCPS.Var "x"), ("field1", f)]
    let prop_obj1 x y = NonCPS.eval obj1 [("x", NonCPS.NumVal x), ("y", NonCPS.NumVal y)] == NonCPS.ObjVal [("field0", NonCPS.NumVal x), ("field1", NonCPS.eval f [("x", NonCPS.NumVal x), ("y", NonCPS.NumVal y)])]
    quickCheck prop_obj1

    putStrLn "\n-------- Object field evaluation: ------------------------"
    let env x y = [("obj0", NonCPS.eval obj0 []), ("obj1", NonCPS.eval obj1 [("x", NonCPS.NumVal x), ("y", NonCPS.NumVal y)])]
    let field00 = NonCPS.Field (NonCPS.Var "obj0") "field0"
    let prop_field01 x y = NonCPS.eval field00 (env x y) == NonCPS.NumVal 42
    quickCheck prop_field01
    let field01 = NonCPS.Field (NonCPS.Var "obj0") "field1"
    let prop_field01 x y = NonCPS.eval field01 (env x y) == NonCPS.NumVal 99
    quickCheck prop_field01
    let field10 = NonCPS.Field (NonCPS.Var "obj1") "field0"
    let prop_field01 x y = NonCPS.eval field10 (env x y) == NonCPS.NumVal x
    quickCheck prop_field01
    let field11 = NonCPS.Field (NonCPS.Var "obj1") "field1"
    let prop_field01 x y = NonCPS.eval field11 (env x y) == NonCPS.FunVal ["x", "y"] (NonCPS.Add (NonCPS.Var "x") (NonCPS.Var "y")) [("x", NonCPS.NumVal x),("y", NonCPS.NumVal y)]
    quickCheck prop_field01

testExplicit :: IO () 
testExplicit = do
    putStrLn "\n-------- Constant evaluation: ----------------------------"
    let s = State { free = 0, store = Map.empty}
    let sid v s = v
    let prop_const0 x = CPS.eval (CPS.Const x) [] s sid == CPS.NumVal x
    quickCheck prop_const0
    let prop_const1 x z = CPS.eval (CPS.Const x) [("z", CPS.NumVal z)] s sid == CPS.NumVal x
    quickCheck prop_const1
    
    putStrLn "\n-------- Variable evaluation: ----------------------------"
    let prop_var x y = CPS.eval (CPS.Var "x") [("x", CPS.NumVal x), ("y", CPS.NumVal y)] s sid == CPS.NumVal x
    quickCheck prop_var

    putStrLn "\n-------- Addition evaluation: ----------------------------"
    let prop_add0 x y = CPS.eval (CPS.Add (CPS.Const x) (CPS.Const y)) [("z", CPS.NumVal 4)] s sid == CPS.NumVal (x+y)
    quickCheck prop_add0
    let prop_add1 x y = CPS.eval (CPS.Add (CPS.Const x) (CPS.Var "y")) [("z", CPS.NumVal 5), ("y", CPS.NumVal y)] s sid == CPS.NumVal (x+y)
    quickCheck prop_add1
    let prop_add2 x y = CPS.eval (CPS.Add (CPS.Var "x") (CPS.Var "y")) [("x", CPS.NumVal x), ("y", CPS.NumVal y)] s sid == CPS.NumVal (x+y)
    quickCheck prop_add2

    putStrLn "\n-------- Function evaluation: ----------------------------"
    let f = CPS.Fun ["x", "y"] (CPS.Add (CPS.Var "x") (CPS.Var "y"))
    let prop_fun0 x y = CPS.eval f [("x", CPS.NumVal x), ("y", CPS.NumVal y)] s sid == CPS.FunVal ["x", "y"] (CPS.Add (CPS.Var "x") (CPS.Var "y")) [("x", CPS.NumVal x), ("y", CPS.NumVal y)]
    quickCheck prop_fun0

    putStrLn "\n-------- Function application evaluation: ----------------"
    let prop_funapp0 x y = CPS.eval (CPS.App f [CPS.Var "x", CPS.Var "y"]) [("x", CPS.NumVal x), ("y", CPS.NumVal y)] s sid == CPS.eval (CPS.Add (CPS.Const x) (CPS.Const y)) [] s sid 
    quickCheck prop_funapp0
    
    putStrLn "\n-------- Object evaluation: ------------------------------"
    let obj0 = CPS.Obj [("field0", CPS.Const 42), ("field1", CPS.Const 99)]
    let prop_obj0 = CPS.eval obj0 [] s sid == CPS.PtrVal 1
    quickCheck prop_obj0
    let obj1 = CPS.Obj [("field0", CPS.Var "x"), ("field1", f)]
    let prop_obj1 = CPS.eval obj1 [] s sid == CPS.PtrVal 2
    print (CPS.eval obj1 [] s sid)
    quickCheck prop_obj1

    -- putStrLn "\n-------- Object field evaluation: ------------------------"
    -- let env x y = [("obj0", CPS.eval obj0 [] s sid), ("obj1", CPS.eval obj1 [("x", CPS.NumVal x), ("y", CPS.NumVal y)] s sid)]
    -- let field00 = CPS.Field (CPS.Var "obj0") "field0"
    -- let prop_field01 x y = CPS.eval field00 (env x y) s sid == CPS.NumVal 42
    -- quickCheck prop_field01
    -- let field01 = CPS.Field (CPS.Var "obj0") "field1"
    -- let prop_field01 x y = CPS.eval field01 (env x y) s sid == CPS.NumVal 99
    -- quickCheck prop_field01
    -- let field10 = CPS.Field (CPS.Var "obj1") "field0"
    -- let prop_field01 x y = CPS.eval field10 (env x y) s sid == CPS.NumVal x
    -- quickCheck prop_field01
    -- let field11 = CPS.Field (CPS.Var "obj1") "field1"
    -- let prop_field01 x y = CPS.eval field11 (env x y) s sid == CPS.FunVal ["x", "y"] (CPS.Add (CPS.Var "x") (CPS.Var "y")) [("x", CPS.NumVal x),("y", CPS.NumVal y)]
    -- quickCheck prop_field01


testMonadic :: IO () -- (runContT ... Just) kanns doch nicht sein TODO
testMonadic = do
    putStrLn "\n-------- Constant evaluation: ----------------------------"
    let prop_const0 x = runContT (Monadic.eval (Monadic.Const x) []) Just == Just (Monadic.NumVal x)
    quickCheck prop_const0
    let prop_const1 x z = runContT (Monadic.eval (Monadic.Const x) [("z", Monadic.NumVal z)]) Just == Just (Monadic.NumVal x)
    quickCheck prop_const1
    
    putStrLn "\n-------- Variable evaluation: ----------------------------"
    let prop_var x y = runContT (Monadic.eval (Monadic.Var "x") [("x", Monadic.NumVal x), ("y", Monadic.NumVal y)]) Just == Just (Monadic.NumVal x)
    quickCheck prop_var

    putStrLn "\n-------- Addition evaluation: ----------------------------"
    let prop_add0 x y = runContT (Monadic.eval (Monadic.Add (Monadic.Const x) (Monadic.Const y)) [("z", Monadic.NumVal 4)]) Just == Just (Monadic.NumVal (x+y))
    quickCheck prop_add0
    let prop_add1 x y = runContT (Monadic.eval (Monadic.Add (Monadic.Const x) (Monadic.Var "y")) [("z", Monadic.NumVal 5), ("y", Monadic.NumVal y)]) Just == Just (Monadic.NumVal (x+y))
    quickCheck prop_add1
    let prop_add2 x y = runContT (Monadic.eval (Monadic.Add (Monadic.Var "x") (Monadic.Var "y")) [("x", Monadic.NumVal x), ("y", Monadic.NumVal y)]) Just == Just (Monadic.NumVal (x+y))
    quickCheck prop_add2

    putStrLn "\n-------- Function evaluation: ----------------------------"
    let f = Monadic.Fun ["x", "y"] (Monadic.Add (Monadic.Var "x") (Monadic.Var "y"))
    let prop_fun0 x y = runContT (Monadic.eval f [("x", Monadic.NumVal x), ("y", Monadic.NumVal y)]) Just == Just (Monadic.FunVal ["x", "y"] (Monadic.Add (Monadic.Var "x") (Monadic.Var "y")) [("x", Monadic.NumVal x), ("y", Monadic.NumVal y)])
    quickCheck prop_fun0

    putStrLn "\n-------- Function application evaluation: ----------------"
    let prop_funapp0 x y = runContT (Monadic.eval (Monadic.App f [Monadic.Var "x", Monadic.Var "y"]) [("x", Monadic.NumVal x), ("y", Monadic.NumVal y)]) Just == runContT (Monadic.eval (Monadic.Add (Monadic.Const x) (Monadic.Const y)) []) Just
    quickCheck prop_funapp0
    
    putStrLn "\n-------- Object evaluation: ------------------------------"
    let obj0 = Monadic.Obj [("field0", Monadic.Const 42), ("field1", Monadic.Const 99)]
    let prop_obj0 x = runContT (Monadic.eval obj0 [("x", Monadic.NumVal x)]) Just == Just (Monadic.ObjVal [("field0", Monadic.NumVal 42), ("field1", Monadic.NumVal 99)])
    quickCheck prop_obj0
    let obj1 = Monadic.Obj [("field0", Monadic.Var "x"), ("field1", f)]
    let fVal x y = fromMaybe (error "No function value") (runContT (Monadic.eval f [("x", Monadic.NumVal x), ("y", Monadic.NumVal y)]) Just)
    let prop_obj1 x y = runContT (Monadic.eval obj1 [("x", Monadic.NumVal x), ("y", Monadic.NumVal y)]) Just == Just (Monadic.ObjVal [("field0", Monadic.NumVal x), ("field1", fVal x y)])
    quickCheck prop_obj1

    putStrLn "\n-------- Object field evaluation: ------------------------"
    let objVal0 = fromMaybe (error "No object value") (runContT (Monadic.eval obj0 []) Just)
    let objVal1 x y = fromMaybe (error "nothing") (runContT (Monadic.eval obj1 [("x", Monadic.NumVal x), ("y", Monadic.NumVal y)]) Just)
    let env x y = [("obj0", objVal0), ("obj1", objVal1 x y)]
    let field00 = Monadic.Field (Monadic.Var "obj0") "field0"
    let prop_field01 x y = runContT (Monadic.eval field00 (env x y)) Just == Just (Monadic.NumVal 42)
    quickCheck prop_field01
    let field01 = Monadic.Field (Monadic.Var "obj0") "field1"
    let prop_field01 x y = runContT (Monadic.eval field01 (env x y)) Just == Just (Monadic.NumVal 99)
    quickCheck prop_field01
    let field10 = Monadic.Field (Monadic.Var "obj1") "field0"
    let prop_field01 x y = runContT (Monadic.eval field10 (env x y)) Just == Just (Monadic.NumVal x)
    quickCheck prop_field01
    let field11 = Monadic.Field (Monadic.Var "obj1") "field1"
    let prop_field01 x y = runContT (Monadic.eval field11 (env x y)) Just == Just (Monadic.FunVal ["x", "y"] (Monadic.Add (Monadic.Var "x") (Monadic.Var "y")) [("x", Monadic.NumVal x),("y", Monadic.NumVal y)])
    quickCheck prop_field01