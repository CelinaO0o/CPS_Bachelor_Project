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
    putStrLn "\n-------- Constant evaluation: ----------------------------"
    let prop_const0 x = NonCPS.eval (NonCPS.Const x) [] == NonCPS.NumVal x
    quickCheck prop_const0
    let prop_const1 x z = NonCPS.eval (NonCPS.Const x) [("z", NonCPS.NumVal z)] ==  NonCPS.NumVal x
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
    let fVal x y = NonCPS.FunVal ["x", "y"] (NonCPS.Add (NonCPS.Var "x") (NonCPS.Var "y")) [("x", NonCPS.NumVal x), ("y", NonCPS.NumVal y)]
    let prop_fun0 x y = NonCPS.eval f [("x", NonCPS.NumVal x), ("y", NonCPS.NumVal y)] == fVal x y 
    quickCheck prop_fun0

    putStrLn "\n-------- Function application evaluation: ----------------"
    let res x y = NonCPS.eval (NonCPS.Add (NonCPS.Const x) (NonCPS.Const y)) [("x", NonCPS.NumVal x), ("y", NonCPS.NumVal y)]
    let prop_funapp0 x y = NonCPS.eval (NonCPS.App f [NonCPS.Var "x", NonCPS.Var "y"]) [("x", NonCPS.NumVal x), ("y", NonCPS.NumVal y)] == res x y
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
    let s = CPS.State {CPS.free = 0, CPS.store = Map.empty}
    let id' v s = (v, s)

    putStrLn "\n-------- Constant evaluation: ----------------------------"
    let prop_const0 x = CPS.eval (CPS.Const x) Map.empty s id' == (CPS.NumVal x, s)
    quickCheck prop_const0
    let prop_const1 x z = CPS.eval (CPS.Const x) (Map.fromList [("z", CPS.NumVal z)]) s id' == (CPS.NumVal x,s)
    quickCheck prop_const1

    putStrLn "\n-------- Variable evaluation: ----------------------------"
    let prop_var x y = CPS.eval (CPS.Var "x") (Map.fromList [("x", CPS.NumVal x), ("y", CPS.NumVal y)]) s id' == (CPS.NumVal x,s)
    quickCheck prop_var

    putStrLn "\n-------- Addition evaluation: ----------------------------"
    let prop_add0 x y = CPS.eval (CPS.Add (CPS.Const x) (CPS.Const y)) (Map.fromList [("z", CPS.NumVal 4)]) s id' == (CPS.NumVal (x+y),s)
    quickCheck prop_add0
    let prop_add1 x y = CPS.eval (CPS.Add (CPS.Const x) (CPS.Var "y")) (Map.fromList [("z", CPS.NumVal 5), ("y", CPS.NumVal y)]) s id' == (CPS.NumVal (x+y),s)
    quickCheck prop_add1
    let prop_add2 x y = CPS.eval (CPS.Add (CPS.Var "x") (CPS.Var "y")) (Map.fromList [("x", CPS.NumVal x), ("y", CPS.NumVal y)]) s id' == (CPS.NumVal (x+y),s)
    quickCheck prop_add2

    putStrLn "\n-------- Function evaluation: ----------------------------"
    let f = CPS.Fun ["x", "y"] (CPS.Add (CPS.Var "x") (CPS.Var "y"))
    let res x y = CPS.FunVal ["x", "y"] (CPS.Add (CPS.Var "x") (CPS.Var "y")) (Map.fromList [("x", CPS.NumVal x), ("y", CPS.NumVal y)])
    let prop_fun0 x y = CPS.eval f (Map.fromList [("x", CPS.NumVal x), ("y", CPS.NumVal y)]) s id' == (res x y, s)
    quickCheck prop_fun0

    putStrLn "\n-------- Function application evaluation: ----------------"
    let res x y = CPS.eval (CPS.Add (CPS.Const x) (CPS.Const y)) Map.empty s id'
    let prop_funapp0 x y = CPS.eval (CPS.App f [CPS.Var "x", CPS.Var "y"]) (Map.fromList [("x", CPS.NumVal x), ("y", CPS.NumVal y)]) s id' == res x y
    quickCheck prop_funapp0

    putStrLn "\n-------- Object evaluation: ------------------------------"
    let obj0 x = CPS.Obj (Map.fromList [("field0", CPS.Const x), ("field1", CPS.Var "x")])
    let env x = Map.fromList [("x", CPS.NumVal x)]
    let constVal x = CPS.eval (CPS.Const x) (env x) s id'
    let varVal x = CPS.eval (CPS.Var "x") (env x) s id'
    let obj0Val x = CPS.ObjVal $ Map.fromList [("field0", fst (constVal x)),("field1", fst (varVal x))]
    let store' x = Map.fromList [(0, obj0Val x)]
    let s' x = CPS.State {CPS.free = 1, CPS.store = store' x}
    let prop_obj0 x = CPS.eval (obj0 x) (env x) s id' == (CPS.PtrVal 0, s' x)
    quickCheck prop_obj0

    let obj1 = CPS.Obj (Map.fromList [("field2", f)])
    let fVal x = CPS.eval f Map.empty (s' x) id'
    let obj1Val x = CPS.ObjVal $ Map.fromList [("field2", fst (fVal x))]
    let store'' x = Map.insert 1 (obj1Val x) (store' x)
    let s'' x = CPS.State {CPS.free = 2, CPS.store = store'' x}
    let prop_obj1 x = CPS.eval obj1 Map.empty (s' x) id' == (CPS.PtrVal 1, s'' x)
    quickCheck prop_obj1

    -- putStrLn "\n-------- Object field evaluation: ------------------------"
    -- let env x = Map.fromList [("obj0", fst $ CPS.eval (obj0 x) (env x) s id'), ("obj1", fst $ CPS.eval obj1 (env x) (s'' x) id')]
    -- let field0 = CPS.Field (CPS.Var "obj0") "field0"
    -- let prop_field0 x = CPS.eval field0 (env x) (s'' x) id' == (CPS.NumVal x, s'' x)
    -- quickCheck prop_field0
    -- let field1 = CPS.Field (CPS.Var "obj0") "field1"
    -- let prop_field1 x = CPS.eval field1 (env x) (s'' x) id' == (CPS.NumVal x, s'' x)
    -- quickCheck prop_field1
    -- let field2 = CPS.Field (CPS.Var "obj1") "field2"
    -- print $ CPS.eval field2 (env 1) (s'' 1) id'
    -- let prop_field2 x = CPS.eval field2 (env x) (s'' x) id' == CPS.eval f (env x) (s'' x) id'
    -- quickCheck prop_field2

    putStrLn "\n-------- Set Object field: -------------------------------"
    let obj0 = CPS.Obj $ Map.fromList [("field", CPS.Const 3)]
    let (CPS.PtrVal address0, s') = CPS.eval obj0 Map.empty s id'
    let env = Map.fromList [("obj0", CPS.PtrVal address0)]
    let setField = CPS.SetField (CPS.Var "obj0") "field" (CPS.Const 4)
    let s'' = CPS.State {CPS.free = 1, CPS.store = Map.fromList [(0, CPS.ObjVal (Map.fromList [("field", CPS.NumVal 4)]))]}
    let prop_setField = CPS.eval setField env s' id' == (CPS.NumVal 4, s'')
    quickCheck prop_setField

    let env = Map.fromList [("z", CPS.NumVal 1), ("x", CPS.NumVal 2), ("y", CPS.NumVal 3)]
    let f = CPS.Fun ["x", "y"] (CPS.Add (CPS.Var "x") (CPS.Var "y"))
    let obj = CPS.Obj $ Map.fromList [("field0", CPS.Var "z"), ("field1", f)]
    let (CPS.PtrVal address, s') = CPS.eval obj env s id'
    let env' = Map.insert "obj" (CPS.PtrVal address) env
    let setField = CPS.SetField (CPS.Var "obj") "field0" (CPS.Var "x")
    let s'' = CPS.State {CPS.free = 1, CPS.store = Map.fromList [(0, CPS.ObjVal (Map.fromList [("field0", CPS.NumVal 2), ("field1", fst $ CPS.eval f env s' id')]))]}
    let prop_setField = CPS.eval setField env' s' id' == (CPS.NumVal 2, s'')
    quickCheck prop_setField
    let setField1 = CPS.SetField (CPS.Var "obj") "field1" (CPS.App f [CPS.Var "x", CPS.Var "y"])
    let fVal = fst $ CPS.eval (CPS.App f [CPS.Var "x", CPS.Var "y"]) env s' id'
    let s''' = CPS.State {CPS.free = 1, CPS.store = Map.fromList [(0, CPS.ObjVal (Map.fromList [("field0", CPS.NumVal 2), ("field1", fVal)]))]}
    let prop_setField1 = CPS.eval setField1 env' s'' id' == (fVal, s''')
    quickCheck prop_setField1


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
