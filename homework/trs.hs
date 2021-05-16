import Data.List

data Term = V String | F String [Term] deriving Eq
type Position = [Int]
instance Show Term where
  show (V x) = x
  show (F f ts) = f ++ "(" ++ intercalate "," [show t | t <- ts] ++ ")"

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
subtermAt _ _ = _

-- replace t u p = t[u]_p
replace :: Term -> Term -> Position -> Term
replace (F f ts) s (i : q) = [ if i == j then .. else .. | (j, tj) <- ... ]

type Subst = [(String, Term)]
-- substitute t σ= tσ
substitute :: Term -> Subst -> Term
substitute (V x) [] = V x
substitute (V x) ((s, t) : subs)
  | x == s = t
  | otherwise = substitute (V x) subs
substitute (F f ts) [] = (F f ts)
substitute (F f ts) ((s, t) : subs) = ??
