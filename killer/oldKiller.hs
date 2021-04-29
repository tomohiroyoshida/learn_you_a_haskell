import System.Environment

main :: IO ()
main = do
  (file : _) <- getArgs
  str <- readFile file
  writeFile "output.smt2"
    (declareFuns allVars
    ++ assert range
    ++ assert square
    ++ assert colFormula
    ++ assert rowFormula
    ++ assert (readCages str)
    ++ checkSat
    ++ getVals allVars)

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
rowFormula = And [Distinct (row n) | n <- [1..9]]

row :: Int -> [Exp]
row i = [Var i j | j <- [1..9]]

-- 各列に被りなし
colFormula :: Formula
colFormula = And [Distinct (col n) | n <- [1..9]]

col :: Int ->  [Exp]
col i = [Var j i | j <- [1..9]]

-- 読み込み
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
allVars = [Var x y | x <- [1..9], y <- [1..9]]

-- 3*3
square :: Formula
square = And (toFs (splitToSquare [Var x y | xys <- prod, x <- left xys, y <- right xys]))

toFs :: [[Exp]] -> [Formula]
toFs ees = [Distinct es | es <- ees]

splitToSquare :: [Exp] -> [[Exp]]
splitToSquare [] = []
splitToSquare es = splitEvery 9 es

splitEvery :: Int -> [Exp] -> [[Exp]]
splitEvery _ [] = []
splitEvery n es = first : (splitEvery n rest)
  where (first,rest) = splitAt n es

left :: ([Int], [Int]) -> [Int]
left (l,_)= l
right :: ([Int], [Int]) -> [Int]
right (_, r) = r

prod :: [([Int], [Int])]
prod = [ (a, b) | a <- nums, b <- nums]

nums :: [[Int]]
nums = [[1,2,3], [4,5,6], [7,8,9]]

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