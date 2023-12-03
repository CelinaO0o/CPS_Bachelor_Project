module ShowAndTell where

import InterpreterCPS as CPS
import InterpreterNonCPS as NCPS
import qualified Data.Map as Map


constants:: IO ()
constants = do
    putStrLn "\n-------- Constant evaluation: ----------------------------"
    putStr "5 = "
    print $ NCPS.eval (NCPS.Const 5) []

variables :: IO ()
variables =  do
    putStrLn "\n-------- Variable evaluation: ----------------------------"
    putStrLn "let x = 1 and y = 2"
    let env = [("x", NCPS.NumVal 1), ("y", NCPS.NumVal 2)]
    putStr "x = "
    print $ NCPS.eval (NCPS.Var "x") env

addition :: IO ()
addition = do 
    putStrLn "\n-------- Addition evaluation: ----------------------------"
    putStr "5 + 2 = "
    print $ NCPS.eval (NCPS.Add (NCPS.Const 5) (NCPS.Const 2)) []
    let env = [("y", NCPS.NumVal 2)]
    putStr "5 + y = "
    print $ NCPS.eval (NCPS.Add (NCPS.Const 5) (NCPS.Var "y")) env

functions :: IO ()
functions = do
    putStrLn "\n-------- Function evaluation: ----------------------------"
    putStrLn "let f = x + y, x = 1 and y = 2"
    let f = NCPS.Fun ["x", "y"] (NCPS.Add (NCPS.Var "x") (NCPS.Var "y"))
    let env = [("x", NCPS.NumVal 1), ("y", NCPS.NumVal 2)]
    putStr "f = "
    print $ NCPS.eval f env

application :: IO ()
application = do 
    putStrLn "\n-------- Function application evaluation: ----------------"
    putStrLn "let f = x + y, x = 1 and y = 2"
    let f = NCPS.Fun ["x", "y"] (NCPS.Add (NCPS.Var "x") (NCPS.Var "y"))
    let env = [("x", NCPS.NumVal 1), ("y", NCPS.NumVal 2)]
    putStr "f(1,2) = "
    print $ NCPS.eval (NCPS.App f [NCPS.Var "x", NCPS.Var "y"]) env

objects :: IO ()
objects = do
    putStrLn "\n-------- Object evaluation: ------------------------------"
    putStrLn "let obj = Object {'field0' = 5, 'field1' = x} and x = 1"
    let obj = NCPS.Obj [("field0", NCPS.Const 5), ("field1", NCPS.Var "x")]
    let env = [("x", NCPS.NumVal 1)]
    putStr "obj = "
    print $ NCPS.eval obj env

fields :: IO ()
fields = do 
    putStrLn "\n-------- Object field evaluation: ------------------------"
    putStrLn "let obj = Object {'field0' = 5, 'field1' = x} and x = 1"
    let obj = NCPS.Obj [("field0", NCPS.Const 5), ("field1", NCPS.Var "x")]
    let env = [("obj", NCPS.eval obj env), ("x", NCPS.NumVal 1)]
    let objfield0 = NCPS.Field (NCPS.Var "obj") "field0"
    let objfield1 = NCPS.Field (NCPS.Var "obj") "field1"
    putStr "objfield0 = "
    print $ NCPS.eval objfield0 env
    putStr "objfield1 = "
    print $ NCPS.eval objfield1 env

compareCPS :: IO ()
compareCPS = do
    
    let s = CPS.State {CPS.free = 0, CPS.store = Map.empty}
    let k v s = (v, s)
    let env = Map.empty

    putStrLn "\n-------- Constant evaluation: ----------------------------"
    putStr "5 = "
    print $ CPS.eval (CPS.Const 5) env s k

    putStrLn "\n-------- Variable evaluation: ----------------------------"
    putStrLn "let x = 1 and y = 2"
    let env = Map.fromList [("x", CPS.NumVal 1), ("y", CPS.NumVal 2)]
    putStr "x = "
    print $ CPS.eval (CPS.Var "x") env s k

    putStrLn "\n-------- Addition evaluation: ----------------------------"
    putStr "5 + 2 = "
    print $ CPS.eval (CPS.Add (CPS.Const 5) (CPS.Const 2)) env s k
    putStr "5 + y = "
    print $ CPS.eval (CPS.Add (CPS.Const 5) (CPS.Var "y")) env s k

    putStrLn "\n-------- Function evaluation: ----------------------------"
    putStrLn "let f = x + y"
    let f = CPS.Fun ["x", "y"] (CPS.Add (CPS.Var "x") (CPS.Var "y"))
    putStr "f = "
    print $ CPS.eval f env s k

    putStrLn "\n-------- Function application evaluation: ----------------"
    putStr "f(1,2) = "
    print $ CPS.eval (CPS.App f [CPS.Var "x", CPS.Var "y"]) env s k

    putStrLn "\n-------- Object evaluation: ------------------------------"
    putStrLn "let obj = Object {'field0' = 5, 'field1' = x}"
    let obj = CPS.Obj $ Map.fromList[("field0", CPS.Const 5), ("field1", CPS.Var "x")]
    putStr "obj = "
    print $ CPS.eval obj env s k

    putStrLn "\n-------- Object field evaluation: ------------------------"
    let (objVal, s') = CPS.eval obj env s k
    let env' = Map.insert "obj" objVal env
    let objfield0 = CPS.Field (CPS.Var "obj") "field0"
    let objfield1 = CPS.Field (CPS.Var "obj") "field1"
    putStr "objfield0 = "
    print $ CPS.eval objfield0 env' s' k
    putStr "objfield1 = "
    print $ CPS.eval objfield1 env' s' k

setfield :: IO ()
setfield = do 
    let s = CPS.State {CPS.free = 0, CPS.store = Map.empty}
    let k v s = (v, s)
    let env = Map.fromList [("x", CPS.NumVal 1), ("y", CPS.NumVal 2)]
    let obj = CPS.Obj $ Map.fromList[("field0", CPS.Const 5), ("field1", CPS.Var "x")]
    let (objVal, s') = CPS.eval obj env s k
    let env' = Map.insert "obj" objVal env

    putStrLn "\n-------- Set Object field: -------------------------------"
    let setField = CPS.SetField (CPS.Var "obj") "field1" (CPS.Var "y")
    putStr "objfield1 = "
    print $ CPS.eval setField env' s' k
