import System.Environment
import Data.List
import System.IO 
import Data.Text.IO as DTI 
import Control.Applicative
import Control.Monad

-- 論理式の実装
data Exp = Var Int Int | Val Int | Plus [Exp]

data Formula = And [Formula]
　　　　　　　　| Or [Formula]
　　　　　　　　| Distinct [Exp]
　　　　　　　　| Geq Exp Exp
　　　　　　　　| Eq Exp Exp

instance Show Exp where
  show (Val n) = show n
  show (Var i j) = "x" ++ show i ++ show j
  show (Plus []) = []
  show (Plus (e : es)) = "(+ " ++ show e ++ " " ++ showEs es ++ ")"

instance Show Formula where
  show (And []) = []
  show (And (f : fs)) = "(and " ++ show f ++ " " ++ showFs fs ++ ")"
  show (Or []) = []
  show (Or (f : fs)) = "(or " ++ show f ++ " " ++ showFs fs ++ ")"
  show (Distinct []) = []
  show (Distinct (e : es)) = "(distinct " ++ show e ++ " " ++ showEs es ++ ")"
  show (Geq e1 e2) = "(>= " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Eq e1 e2) = "(= " ++ show e1 ++ " " ++ show e2 ++ ")"

showEs :: [Exp] -> String
showEs [] = []
showEs [x] = show x
showEs (x : xs) = show x ++ " " ++ showEs xs

showFs :: [Formula] -> String
showFs [] = []
showFs [f] = show f
showFs (f : fs) = show f ++ " " ++ showFs fs


-- 数独ソルバー
cageFormula :: [[Int]] -> Formula
cageFormula xs = And (showCFs xs)

showCFs :: [[Int]] -> [Formula]
showCFs [] = []
showCFs ([] : _) = []
showCFs ((x : xs) : []) = [Eq (Val x) (Plus (vars xs))]
showCFs ((x : xs) : xss) = [Eq (Val x) (Plus (vars xs))]  ++ showCFs xss

vars :: [Int] -> [Exp]
vars [] = []
vars (x : xs) = Var (x `div` 10) (x `mod` 10) : vars xs

-- 行に被りなし
rowFormula :: Formula
rowFormula = And (uniqueRow 1)

uniqueRow :: Int -> [Formula]
uniqueRow n
  | n == 9 = [Distinct (row 9 1)]
  | otherwise = [Distinct (row n 1)] ++ uniqueRow (n+1)

row :: Int -> Int -> [Exp]
row i j
  | j == 9 = [Var i 9]
  | otherwise = Var i j : row i (j+1)


-- 各列に被りなし
colFormula :: Formula
colFormula = And (uniqueCol 1)
uniqueCol :: Int -> [Formula]
uniqueCol n
  | n == 9 = [Distinct (col 1 9)]
  | otherwise = [Distinct (col 1 n)] ++ uniqueCol (n+1)

col :: Int -> Int ->  [Exp]
col i j
  | i == 9 = [Var 9 j]
  | otherwise =  Var i j : col (i+1) j

-- TODO: 3*3に被りがない
-- unique3 :: Formula
-- unique3 = And three

-- 数の値域が1~9
range :: Formula
range = And (oneToNine 11)

oneToNine :: Int -> [Formula]
oneToNine x
  | x == 99 = [one x] ++ [nine x]
  | x `mod` 10 == 0 = oneToNine (x+1)
  | otherwise = [one x] ++ [nine x] ++ oneToNine (x+1)

one :: Int -> Formula
one x = Geq (Var (x `div` 10) (x `mod` 10)) (Val 1)
nine :: Int -> Formula
nine x = Geq (Val 9) (Var (x `div` 10) (x `mod` 10))

-- declare-fun
declareFun :: [Exp] -> String
declareFun [] = ""
declareFun (x : xs) = "(declare-fun " ++ show x ++ " Int)" ++ declareFun xs

-- check-sat
check :: String
check = "(check-sat)"

-- assert
assert :: Formula -> String
assert f = "(assert " ++ show f ++ ")"

-- get-value
getVal :: [Exp] -> String
getVal [] = []
getVal es = "(get-value (" ++ showVals es ++ "))"
showVals :: [Exp] -> String
showVals [] = ""
showVals [e] = show e
showVals (e : es) = show e ++ " " ++ showVals es

-- tests
test3 = Plus [(Val 1), (Var 2 3), (Var 2 3), Val 1, Val 19]
test4 = Plus [(Var 1 2), (Var 3 4)]
test5 = Plus [Plus [(Var 1 2), (Var 3 4)], Plus [(Val 1), (Val 9)]]

test10 = And [Geq (Val 1) (Val 2), Eq (Val 3) (Val 4), Geq (Val 55) (Val 15)]
test11 = Or [Geq (Val 1) (Val 2), Eq (Val 3) (Val 4)]
test12 = Geq (Var 1 2) (Var 3 4)
test13 = Eq (Val 1) (Var 2 3)
test14 = Distinct [Val 1,Val 2,Val 3,Val 4,Val 5]
test15 = And []
test16 = Or []

nums =  [[3, 11, 12], [15, 13, 14, 15], [22, 16, 25, 26, 35 ]]
cage1 = cageFormula nums

as0 = assert (cageFormula nums)
as1 = assert rowFormula
as2 = assert colFormula
as3 = assert range

declare = declareFun [(Var 1 1), (Var 2 3), (Var 2 3)]
che = check
getVals = getVal [(Var 1 1), (Var 2 3), (Var 2 3)]