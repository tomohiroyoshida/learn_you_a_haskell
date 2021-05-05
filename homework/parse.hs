import Text.ParserCombinators.Parsec
import System.Process

-- キラー数独の問題を入力として受け取り、答えを 9x9 の盤上に表示するプログラム
solveSudoku :: FilePath -> IO ()
solveSudoku file = do
  createSMTFile file
  result <- solve "z3" "smt.smt2"
  case result of
    Just xs -> putStr (showBlock 1 [n | (_,n) <- xs])
    Nothing -> putStr ""

createSMTFile :: FilePath -> IO ()
createSMTFile file = do
    str <- readFile file
    writeFile "smt.smt2"
      (declareFuns allVars
      ++ assert range
      ++ assert squareFormula
      ++ assert colFormula
      ++ assert rowFormula
      ++ assert (readCages str)
      ++ checkSat
      ++ getVals allVars)

-- solve
type SMTInput = FilePath
solve :: String -> SMTInput -> IO SMTOutput
solve toolPath input = do
  result <- readProcess toolPath [input] []
  case parse parseSMTOutput "z3result.txt" result of
    Left e -> error (show e)
    Right r -> return r

showBlock ::  Int -> [Int] -> String
showBlock _ [] = ""
showBlock n (x : xs)
  | n `mod` 9 == 0 = show x ++ "\n" ++ showBlock (n+1) xs
  | otherwise = show x ++ " " ++ showBlock (n+1) xs

-- scanners
keyword :: String -> Parser ()
keyword s = do
  spaces
  _ <- string s
  spaces

parseVar :: Parser String
parseVar = do
  spaces
  s <- many1 (noneOf " \n\r\t\0,()")
  spaces
  return s

myParse :: Parser (String, Int)
myParse = do
  keyword "("
  s <- many1 (noneOf " \n\r\t\0,()")
  spaces
  n <- many1 (noneOf " \n\r\t\0,()")
  keyword ")"
  return (s,(read n :: Int))

type SMTOutput = Maybe [(String, Int)]

parseSMTOutput :: Parser SMTOutput
parseSMTOutput = try parseSat <|> parseUnsat

parseSat :: Parser SMTOutput
parseSat = do
  keyword "sat"
  keyword "("
  s <- many myParse
  keyword ")"
  return (Just s)

parseUnsat :: Parser SMTOutput
parseUnsat = do
  keyword  "unsat"
  return Nothing

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
showEFs es = unwords [ show e | e <- es]

-- 数独ソルバー
-- 各行に被りなし
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
readCage s = Eq (Val (toInt (head (words s)))) (Plus [readVar str | str <- tail (words s)])

readCages :: String -> Formula
readCages s = And [readCage cage | cage <- lines s]

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
squareFormula :: Formula
squareFormula = And [Distinct es | es <- [vars xys | xys <- square]]

vars :: [(Int, Int)] -> [Exp]
vars xys = [Var x y | (x,y) <- xys]

square :: [[(Int, Int)]]
square = [[(x,y) | x <- xs, y <- ys] | xs <- nums, ys <- nums]

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