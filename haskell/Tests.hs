module Tests where

import InterpreterNonCPS as NonCPS
import InterpreterCPS as CPS
import InterpreterMonadic as Monadic

import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Cont

main :: IO ()
main = testNonCPS


testNonCPS :: IO ()
testNonCPS = do

    -- let eval = NonCPS.eval 
    --     Const = NonCPS.Const -- won't work TODO

    
    putStrLn "\n-------- Constant evaluation: ----------------------------"
    putStr $ "5 = " ++ show (NonCPS.eval (NonCPS.Const 5) [])
    
    putStrLn "\n-------- Variable evaluation: ----------------------------"
    putStrLn "Let [(x = 3), (y = 7)]"
    putStr $ "x = " ++ show (NonCPS.eval (NonCPS.Var "x") [("x", NonCPS.NumVal 3), ("y", NonCPS.NumVal 7)])
    
    putStrLn "\n-------- Addition evaluation: ----------------------------"
    putStrLn "Let [(x = 3), (y = 5)]"
    putStr "3 + y = "
    print $ NonCPS.eval (NonCPS.Add (NonCPS.Const 3) (NonCPS.Var "y")) [("x", NonCPS.NumVal 3), ("y", NonCPS.NumVal 5)]
    putStr "x + 5 = "
    print $ NonCPS.eval (NonCPS.Add (NonCPS.Var "x") (NonCPS.Const 5)) [("x", NonCPS.NumVal 3), ("y", NonCPS.NumVal 5)]
    putStr "x + y = "
    print $ NonCPS.eval (NonCPS.Add (NonCPS.Var "x") (NonCPS.Var "y")) [("x", NonCPS.NumVal 3), ("y", NonCPS.NumVal 5)]
    
    putStrLn "\n-------- Function evaluation: ----------------------------"
    let f = NonCPS.Fun ["x", "y"] (NonCPS.Add (NonCPS.Var "x") (NonCPS.Var "y")) 
    putStr $ "f(x,y) = " ++ show (NonCPS.eval f [])

    putStrLn "now let [(x = 7)]"
    putStr $ "f(x,y) = " ++ show (NonCPS.eval f [("x", NonCPS.NumVal 7)])
    
    putStrLn "\n-------- Function application evaluation: ----------------"
    putStr "f(x=1,y=2) = "
    print $ NonCPS.eval (NonCPS.App f [NonCPS.Const 1, NonCPS.Const 2]) []

    putStrLn "now let [(x = 7)]"
    putStr "f(x=1,y=2) = "
    print $ NonCPS.eval (NonCPS.App f [NonCPS.Const 1, NonCPS.Const 2]) [("x", NonCPS.NumVal 7)]

    putStrLn "\n-------- Object evaluation: ------------------------------"
    let obj1 = NonCPS.Obj [("field1", NonCPS.Const 42), ("field2", NonCPS.Const 99)]
    putStr "obj1 = "
    print $ NonCPS.eval obj1 []

    putStrLn " now let [(x = 5)]"
    let obj2 = NonCPS.Obj [("field1", NonCPS.Var "x"), ("field2", f)]
    putStr "obj2 = "
    print $ NonCPS.eval obj2 [("x", NonCPS.NumVal 5)]

    putStrLn "\n-------- Object field evaluation: ------------------------"
    putStrLn "let [(obj1 = ...), (obj2 = ...)]"
    let env = [("obj1", NonCPS.eval obj1 []), ("obj2", NonCPS.eval obj2 [("x", NonCPS.NumVal 5)])]
    let field11 = NonCPS.Field (NonCPS.Var "obj1") "field1"
    let field12 = NonCPS.Field (NonCPS.Var "obj1") "field2"
    let field21 = NonCPS.Field (NonCPS.Var "obj2") "field1"
    let field22 = NonCPS.Field (NonCPS.Var "obj2") "field2"
    putStrLn $ "object1 field1: " ++ show (NonCPS.eval field11 env)
    putStrLn $ "object1 field2: " ++ show (NonCPS.eval field12 env)
    putStrLn $ "object2 field1: " ++ show (NonCPS.eval field21 env)
    putStrLn $ "object2 field2: " ++ show (NonCPS.eval field22 env)

testCPS :: IO ()
testCPS = do
    putStrLn "-------- Constant evaluation: ----------------------------"
    print (CPS.eval (CPS.Const 5) [] id)
    putStrLn "-------- Variable evaluation: ----------------------------"
    putStrLn "-------- Addition evaluation: ----------------------------"
    putStrLn "-------- Function evaluation: ----------------------------"
    putStrLn "-------- Function application evaluation: ----------------"
    putStrLn "-------- Object evaluation: ------------------------------"
    putStrLn "-------- Object field evaluation: ------------------------"


testMonadicCPS :: IO ()
testMonadicCPS = do
    putStrLn "-------- Constant evaluation: ----------------------------"
    putStrLn "-------- Variable evaluation: ----------------------------"
    putStrLn "-------- Addition evaluation: ----------------------------"
    putStrLn "-------- Function evaluation: ----------------------------"
    putStrLn "-------- Function application evaluation: ----------------"
    putStrLn "-------- Object evaluation: ------------------------------"
    putStrLn "-------- Object field evaluation: ------------------------"
