import Data.List
import Control.Monad (guard)

data Term = V String | F String [Term] deriving Eq
type Position = [Int]
instance Show Term where
  show (V x) = x
  show (F f ts) = f ++ "(" ++ intercalate "," [show t | t <- ts] ++ ")"
-- Var(t)
-- variables :: Term -> [String]

-- Pos(t)
positions :: Term -> [Position]
positions (V _) = [[]]
positions (F _ ts) =
  [[]] ++ [[l] ++ ps | (l, r) <- zip [0..] ts, ps <- positions r, 0 <= l && l < length ts]

-- subtermAt t p = t|_p
subtermAt :: Term -> Position -> Term
subtermAt t [] = t
subtermAt (F _ ts) (p : ps)
  | 0 <= p && p < length ts = subtermAt (ts !! p) ps
subtermAt _ _ = V ""

-- replace _ u [] = u
-- replace (F f ts) u (p : ps) = do
--     guard (p >= 0 && p < length ts)
--     let (ts1, t : ts2) = splitAt p ts
--     t <- replace t u ps
--     return (F f (ts1 ++ t : ts2))
-- replace _ t _ = t

-- replace t u p = t[u]_p
replace :: Term -> Term -> Position -> Term
replace (V x) t _ = t
replace (F f ts) t [] = t
-- replace (F f ts) t (i : q) = [ if i == j then  else .. | (j, tj) <- ... ]

-- 以下の事項に取り組んで下さい。
-- a. まず右辺の項も左辺と同じくプログラムのデータ構造の表記で書いて頂けますか。
-- b. 前のメールで差分から定義を導出する方法について詳しく説明しました。
--    それをどのように当てはめようとし、どこで詰まっているのか教えて頂けませんか？
-- c. 上記を スライド、あるいは a.pdf の定義に沿って計算してください。
--     計算過程がわかるように書いて下さい。
-- d. c. の計算過程をプログラムの Term 型を用いて記述してください。
-- １つ１０分で４０分でおわると思います。

r =  replace (V "x") (V "z") [] 
-- = V "z"
r2 = replace (F "add" [V "0", V "y"]) (V "z") [0]
-- = F "add" [V "z", V "y"]
r3 = replace (F "add" [V "0", V "y"]) (V "z") [1]
-- = F "add" [V "0", V "z"]
r8 = replace (F "add" [V "0", F "s" [F "add" [F "s" [V "0"], F "s" [V "y"]]]]) (V "z") []
-- = V "z"
r4 = replace (F "add" [V "0", F "s" [F "add" [F "s" [V "0"], F "s" [V "y"]]]]) (V "z") [1]
-- = F "add" [V "0", V "z"]
r5 = replace (F "add" [V "0", F "s" [F "add" [F "s" [V "0"], F "s" [V "y"]]]]) (V "z") [1, 0]
-- = F "add" [V "0", F "s" [V "z"]]
r6 = replace (F "add" [V "0", F "s" [F "add" [F "s" [V "0"], F "s" [V "y"]]]]) (V "z") [1, 0, 0]
= F "add" [V "0", F "s" [F "add" [V "z", F "s" [V "y"]]]]
r7 = replace (F "add" [V "0", F "s" [F "add" [F "s" [V "0"], F "s" [V "y"]]]]) (V "z") [1, 0, 0, 0]
= F "add" [V "0", F "s" [F "add" [F "s" [V "z"], F "s" [V "y"]]]]

replace (F "add" [V "0", F "s" [F "add" [F "s" [V "0"], F "s" [V "y"]]]]) (V "z") [1, 0, 0, 0]
= F "add" [V "0", F "s" [F "add" [F "s" [V "z"], F "s" [V "y"]]]]
= F "add" [V "0", F "s" [F "add" [V "z", F "s" [V "y"]]]]

type Subst = [(String, Term)]
-- substitute t σ= tσ
substitute :: Term -> Subst -> Term
substitute x [] = x
substitute (V x) ((s, t) : subs)
  | x == s = t
  | otherwise = substitute (V x) subs
substitute (F f (te : tes)) ((s, t) : subs)
  | te == s = substitute (F f (t : tes)) ((s, t) : subs)

-- subs1 = substitute (V "x") []
subs2 = substitute (V "x") [("x", F "s" [V "y"])] -- = s(y)
subs3 = substitute (V "y") [("x", F "s" [V "y"]), ("y", F "s" [V "0"])] -- = s(0)
subs4 = substitute (F "add" [V "x", V "y"]) [] -- = add(x, y)
subs5 = substitute (F "add" [V "x", V "y"]) [("x", F "s" [V "y"])] -- = add(s(y), y)
subs6 = substitute (F "add" [V "x", V "y"]) [("x", F "s" [V "y"]), ("y", F "s" [V "0"])] -- = add(s(y), s(0))

-- test
p1 =  positions (V "x")
p2 =  positions (F "add" [V "x", V "y"])                         -- add(x,y)
p3 =  positions (F "add" [F "s" [F "s" [V "x"]], F "s" [V "y"]]) -- add(s(s(x)),s(y))
p4=  positions (F "add" [F "s" [F "add" [V "x", V "y"]], F "s" [V "y"]]) -- add(s(add(x,y)),s(y))

sub1 = subtermAt (F "add" [F "s" [F "s" [V "x"]], F "s" [V "y"]]) [0,0]
                  -- add(s(s(x)),s(y))

z = zip [0..] [V "x"]
z1 = zip [0..] [F "add1" [V "x", V "y"]]
z2 = zip [0..] [F "add1" [F "s" [V "x"], V "y"]]