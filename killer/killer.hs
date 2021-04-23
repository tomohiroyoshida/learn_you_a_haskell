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


-- 各列に被りがない
colFormula :: Formula
colFormula = And (uniqueCol 1)
uniqueCol :: Int -> [Formula]
uniqueCol n
  | n == 9 = [Distinct (col 9 1)]
  | otherwise = [Distinct (col 1 n)] ++ uniqueCol (n+1)

col :: Int -> Int ->  [Exp]
col i j
  | i == 9 = [Var 9 j]
  | otherwise =  Var i j : col (i+1) j

-- 3*3に被りがない
-- unique3 :: String
-- unique3 = "(distinct " ++

-- 数は1~9(今回はx=11代入で正しく作動)
oneToNine :: String
oneToNine = range 11
range :: Int -> String
range x
  | x == 99 = one x ++ nine x
  | x `mod` 10 == 0 = range (x+1)
  | otherwise = one x ++ nine x ++ range (x+1)

one :: Int -> String
one x = show (Geq (Var (x `div` 10) (x `mod` 10)) (Val 1))
nine :: Int -> String
nine x = show (Geq (Val 9) (Var (x `div` 10) (x `mod` 10)))

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
getVal [] = ""
getVal es = "(get-value (" ++ showVals es ++ ")"
showVals :: [Exp] -> String
showVals [] = ""
showVals [e] = show e
showVals (e : es) = show e ++ " " ++ showVals es

-- main :: IO ()
-- main = do
--   ss <- (map read . words) <$> getLine
-- 	print ss

-- printList :: Handle -> IO ()
-- printList fp = do
--     eof <- hIsEOF fp
--     if eof then return ()
--     else do
--       str <- DTI.hGetLine fp
--       print str
--       printList fp
-- main :: IO ()
-- main = do
--     fp <- openFile "input.txt" ReadMode
--     printList fp

-- [a, b] <- map read . words <$> getLine

-- tests
test1 = Var 1 2
test2 = Val 1
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
dis = "(distinct x11 x21 x31 x41 x51 x61 x71 x81 x91) (distinct x12 x22 x32 x42 x52 x62 x72 x82 x92) (distinct x13 x23 x33 x43 x53 x63 x73 x83 x93) (distinct x14 x24 x34 x44 x54 x64 x74 x84 x94) (distinct x15 x25 x35 x45 x55 x65 x75 x85 x95) (distinct x16 x26 x36 x46 x56 x66 x76 x86 x96) (distinct x17 x27 x37 x47 x57 x67 x77 x87 x97) (distinct x18 x28 x38 x48 x58 x68 x78 x88 x98) (distinct x19 x29 x39 x49 x59 x69 x79 x89 x99)"
testa = cageFormula nums
testb = assert (cageFormula nums)
as1 = assert rowFormula
as2 = assert rowFormula
