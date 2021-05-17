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

-- replace t u p = t[u]_p
-- replace :: Term -> Term -> Position -> Term
-- replace (V x) t [] = t
-- replace (V x) t (p : ps) = t
-- replace (F f ts) t [] = t
-- replace _ _ _ = [V "ERR"]

-- r1 = replace (F "add" [F "s" [F "add" [V "x", V "y"]], F "s" [V "y"]]) (V "xy") [0]
-- replace _ u [] = u
-- replace (F f ts) u (p : ps) = do
--     guard (p >= 0 && p < length ts)
--     let (ts1, t : ts2) = splitAt p ts
--     t <- replace t u ps
--     return (F f (ts1 ++ t : ts2))
-- replace _ t _ = t

type Subst = [(String, Term)]
-- substitute t σ= tσ
substitute :: Term -> Subst -> Term
substitute x [] = x
substitute (V x) ((s, t) : subs)
  | x == s = t
  | otherwise = substitute (V x) subs
substitute (F f (te : tes)) ((s, t) : subs) = V "x"

ss1 = substitute (V "x") [("x", (V "changeX"))]
ss2 = substitute (V "x") [("x", (F "changeX" [])), ("y", (V "changeY"))]

ss4 = substitute (F "add" []) []
ss5 = substitute (F "add" [(V "a"), (V "b")]) [] -- add(a,b)
ss6 = substitute (F "add" [(V "a"), (V "b")]) [("a", (V "A"))] -- add(A,b)
ss7 = substitute (F "add" [(V "a"), (V "b")]) [("a", (V "A")), ("b", (V "B"))] -- add(A,B)
ss8 = substitute (F "add" [(F "add" [(V "a"), (V "b")]), (F "add" [(V "a"), (V "b")])]) [("a", (V "A")), ("b", (V "B"))] 
-- add(add(A,B),add(A,B))

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