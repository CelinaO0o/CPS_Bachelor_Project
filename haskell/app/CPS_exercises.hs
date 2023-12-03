module CPS_exercises where

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

factorialCps :: Integral a => a -> (a -> b) -> b
factorialCps 0 cont = cont 1
factorialCps n cont = factorialCps (n-1) (\x -> cont (n * x))
    
-- TCO pushes info as lambdas onto heap 
fibonacci :: Integral a => a -> a
fibonacci n 
    | n < 2 = 1
    | otherwise = fibonacci (n-1) + fibonacci (n-2)

fibonacciCps :: Integral a => a -> (a -> b) -> b
fibonacciCps n cont
    | n < 2 = cont 1
    | otherwise = fibonacciCps (n-1) (\left -> fibonacciCps (n-2) (\right -> cont (left + right)))


removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst s (x:xs)
    | s == x    = xs
    | otherwise = x : removeFirst s xs

removeFirstCPS :: Eq a => a -> [a] -> ([a] -> b) -> b
removeFirstCPS _ [] k = k []
removeFirstCPS s (x:xs) k
    | s == x    = k xs
    | otherwise = removeFirstCPS s xs (\r -> k(x : r))


listSum :: Integral a => [a] -> a
listSum [] = 0
listSum (x:xs) = x + listSum xs

listSumCPS :: Integral a => [a] -> (a -> b) -> b
listSumCPS [] k = k 0
listSumCPS (x:xs) k = listSumCPS xs (\r -> k (x + r))


type Sym = Char
data Exp = Var Sym | Lambda Sym Exp | App Exp Exp

occursFree :: Sym -> Exp -> Bool
occursFree var (Var x) = var == x
occursFree var (Lambda y e) = var /= y && occursFree var e
occursFree var (App e1 e2) = occursFree var e1 || occursFree var e2

occursFreeCPS :: Sym -> Exp -> (Bool -> r) -> r
occursFreeCPS var (Var x) k = k (var == x)
occursFreeCPS var (Lambda y e) k = occursFreeCPS var e (\k1 -> k (var /= y && k1))
occursFreeCPS var (App e1 e2) k = occursFreeCPS var e1 (\k2 -> occursFreeCPS var e2 (\k3 -> k(k2 || k3)))


subst :: Char -> Char -> [Char] -> [Char]
subst new old [] = []
subst new old (x:xs)
    | x == old = new : subst new old xs
    | otherwise = x : subst new old xs

substCPS :: Char -> Char -> [Char] -> ([Char] -> r) -> r
substCPS new old [] k = k []
substCPS new old (x:xs) k
    | x == old = substCPS new old xs (\k1 -> k (new : k1))
    | otherwise =substCPS new old xs (\k1 -> k (x : k1))


-- q a k = k (-a)
-- p a b k = k (a * b)
-- \x y -> p (+ 8 x) (q y) -- let p and q be predefined CPS functions
-- CPS version
-- \x y k -> q y (\k1 -> p (+ 8 x) k1 k) -- can (+ 8 x) be treated as a variable? yes
-- rewrite function to take one argument, each lambda takes own k