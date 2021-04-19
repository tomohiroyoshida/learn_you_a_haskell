-- 論理式の実装
data Exp = Var Int Int | Val Int | Plus [Exp]

data Formula = And [Formula]
　　　　　　　　| Or [Formula]
　　　　　　　　| Distinct [Exp]
　　　　　　　　| Geq Exp Exp
　　　　　　　　| Eq Exp Exp

showEs :: [Exp] -> String
showEs [] = ""
showEs [x] = show x
showEs (x : xs) = show x ++ " " ++ showEs xs

showFs :: [Formula] -> String
showFs [] = ""
showFs [f] = show f
showFs (f: fs) = show f ++ " " ++ showFs fs

instance Show Exp where
  show (Val n) = show n
  show (Var i j) = "x" ++ show i ++ show j
  show (Plus []) = ""
  show (Plus (e : es)) = "(+" ++ " " ++ show e ++ " " ++ showEs es ++ ")"

instance Show Formula where
  show (And []) = ""
  show (And (f : fs)) = "(and " ++ show f ++ " " ++ showFs fs ++ ")"
  show (Or []) = ""
  show (Or (f : fs)) = "(or " ++ show f ++ " " ++ showFs fs ++ ")"
  show (Distinct []) = ""
  show (Distinct (e : es)) = "(distinct " ++ show e ++ " " ++ showEs es ++ ")"
  show (Geq e1 e2) = "(>= " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Eq e1 e2) = "(= " ++ show e1 ++ " " ++ show e2 ++ ")"
