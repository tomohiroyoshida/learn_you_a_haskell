-- 次の関数を各々型を書き実装してください
readVar :: String -> Exp
readVar s = Var (toInt s `div` 10) (toInt s `mod` 10)

readCage :: String -> Formula
readCage s = Eq (Val (toInt (h (words s)))) (Plus (toVars (t (words s))))

readCages :: String -> Formula
readCages s = And (toCages (lines s))

-- 補助関数
toInt :: String -> Int
toInt s = (read s :: Int)

toCages :: [String] -> [Formula]
toCages [] = []
toCages (s : ss) = readCage s : toCages ss

toVars :: [String] -> [Exp]
toVars [] = []
toVars (s : ss) = readVar s : toVars ss

h :: [String] -> String
h [] = []
h (s:_) = s

t :: [String] -> [String]
t [] = []
t (_:ss) = ss


-- Exp Formula
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
  show (Plus (e : es)) = "(+ " ++ show e ++ " " ++ shows' es ++ ")"

instance Show Formula where
  show (And []) = "true"
  show (And (f : fs)) = "(and " ++ show f ++ " " ++ shows' fs ++ ")"
  show (Or []) = "false"
  show (Or (f : fs)) = "(or " ++ show f ++ " " ++ shows' fs ++ ")"
  show (Distinct []) = ""
  show (Distinct (e : es)) = "(distinct " ++ show e ++ " " ++ shows' es ++ ")"
  show (Geq e1 e2) = "(>= " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Eq e1 e2) = "(= " ++ show e1 ++ " " ++ show e2 ++ ")"

shows' :: Show a => [a] -> String
shows' [] = []
shows' es = unwords [ show e | e <- es]
