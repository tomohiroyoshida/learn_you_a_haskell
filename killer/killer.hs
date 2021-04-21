import Data.List

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
  show (Plus (e : es)) = "(+ " ++ show e ++ " " ++ showPls es ++ ")"

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

showPls :: [Exp] -> String
showPls xs = intercalate " " [showEs xs]


-- 数独ソルバー
cageFormula :: [[Int]] -> String
cageFormula ((xs)) = "(and " ++ showCFs xs ++ ")"

showCFs :: [[Int]] -> String
showCFs [] = []
showCFs ([] : _) = []
showCFs ((x : xs) : []) = show (Eq (Val x) (Plus (vars xs)))
showCFs ((x : xs) : xss) =
  show (Eq (Val x) (Plus (vars xs))) ++ " " ++ showCFs xss

vars :: [Int] -> [Exp]
vars [] = []
vars (x : xs) = Var (x `div` 10) (x `mod` 10) : vars xs

-- 各行に
-- unique ::

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

testa = cageFormula [[3, 11, 12], [15, 13, 14, 15]]