import System.Environment

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
  show (Plus (e : es)) = "(+ " ++ show e ++ " " ++ showEFs es ++ ")"

instance Show Formula where
  show (And []) = "true"
  show (And (f : fs)) = "(and " ++ show f ++ " " ++ showEFs fs ++ ")"
  show (Or []) = "false"
  show (Or (f : fs)) = "(or " ++ show f ++ " " ++ showEFs fs ++ ")"
  show (Distinct []) = ""
  show (Distinct (e : es)) = "(distinct " ++ show e ++ " " ++ showEFs es ++ ")"
  show (Geq e1 e2) = "(>= " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Eq e1 e2) = "(= " ++ show e1 ++ " " ++ show e2 ++ ")"

showEFs :: Show a => [a] -> String
showEFs [] = []
showEFs es = unwords [ show e | e <- es]

-- 数独ソルバー

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

readVar :: String -> Exp
readVar s = Var (toInt s `div` 10) (toInt s `mod` 10)

readCage :: String -> Formula
readCage s = Eq (Val (toInt (head (words s)))) (Plus (toVars (tail (words s))))

readCages :: String -> Formula
readCages s = And (toCages (lines s))

toCages :: [String] -> [Formula]
toCages [] = []
toCages (s : ss) = readCage s : toCages ss

toVars :: [String] -> [Exp]
toVars [] = []
toVars (s : ss) = readVar s : toVars ss

toInt :: String -> Int
toInt s = (read s :: Int)

-- 数の値域が1~9
range :: Formula
range = And (oneToNine allVars)

oneToNine :: [Exp] -> [Formula]
oneToNine [] = []
oneToNine (e : es) = Geq e (Val 1) : Geq (Val 9) e : oneToNine es

-- 全ての変数(x11~x99)
allVars :: [Exp]
allVars = getAllVars 11

getAllVars :: Int -> [Exp]
getAllVars n
  | n == 99 = [Var 9 9]
  | n `mod` 10 == 0 = getAllVars (n+1)
  | otherwise = Var (n `div` 10) (n `mod` 10) : getAllVars (n+1)

-- declare-fun
declareFuns :: [Exp] -> String
declareFuns [] = ""
declareFuns (x : xs) = "(declare-fun " ++ show x ++ " () Int) " ++ declareFuns xs

-- check-sat
checkSat :: String
checkSat = "(check-sat)"

-- assert
assert :: Formula -> String
assert f = "(assert " ++ show f ++ ")"

-- get-value
getVals :: [Exp] -> String
getVals [] = []
getVals es = "(get-value (" ++ showVals es ++ "))"
showVals :: [Exp] -> String
showVals [] = ""
showVals (e : es) = show e ++ " " ++ showVals es

main :: IO ()
main = do
  (file : _) <- getArgs
  str <- readFile file
  writeFile "result.smt2"
    ((declareFuns allVars)
    ++ "\n"
    ++ (assert unique)
    ++ "\n"
    ++ (assert colFormula)
    ++ "\n"
    ++ (assert rowFormula)
    ++ "\n"
    ++ (assert range)
    ++ "\n"
    ++ (assert (readCages str))
    ++ "\n"
    ++ checkSat
    ++ "\n"
    ++ (getVals allVars))

  print (
        declareFuns (getAllVars 11)
        ++ (assert (readCages str))
        ++ (assert range)
        ++ checkSat
        ++ (getVals (getAllVars 11))
        )

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

numlist =  [[3, 11, 12], [15, 13, 14, 15], [22, 16, 25, 26, 35 ]]

as1 = assert rowFormula
as2 = assert colFormula
as3 = assert range

declare = declareFuns [(Var 1 1), (Var 2 3), (Var 2 3)]
che = checkSat
-- getVals = getVal [(Var 1 1), (Var 2 3), (Var 2 3)]


aa = [1,2,3]
bb  = [1,2,3]
bar = [Var a b | a <- aa, b <- bb]

-- nums = [[1,2], [10,11]]
nums :: [[Int]]
nums = [[1,2,3], [4,5,6], [7,8,9]]

prd :: [([Int], [Int])]
prd = [ (as, bs) | as <- nums, bs <- nums]

vars :: [Exp]
vars = [Var a b | as <- prd, a <- fst as, b <- snd as]

splitEvery :: Int -> [Exp] -> [[Exp]]
splitEvery _ [] = []
splitEvery n es = first : (splitEvery n rest)
  where (first,rest) = splitAt n es

splitToCage :: [Exp] -> [[Exp]]
splitToCage [] = []
splitToCage es = splitEvery 9 es

toFs :: [[Exp]] -> [Formula]
toFs [] = []
toFs (xs: xxs) = Distinct xs : toFs xxs

unique :: Formula
unique = And (toFs (splitToCage vars))
-- nn = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]