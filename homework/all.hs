import Data.List

-- work_1
-- 問題1
myGcd :: Int -> Int -> Int
myGcd x 0 = x
myGcd x y
  | x < y = myGcd y x
  | otherwise = myGcd y (x `mod` y)

-- 問題2
myRange :: Int -> Int -> [Int]
myRange x y
  | x > y = []
  | x == y = [x]
  | otherwise = x:myRange (x+1) y

-- 問題3
myInsert :: Int -> [Int] -> [Int]
myInsert x [] = [x]
myInsert x (y:ys)
  | x <= y = x:y:ys
  | otherwise = y:myInsert x ys

-- 問題4
myIsort :: [Int] -> [Int]
myIsort [] = []
myIsort (x:xs) = myInsert x (myIsort xs)


-- work_2
-- 問題1
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr k z = go
  where
    go [] = z
    go (y:ys) = y `k` go ys

sumList :: [Int] -> Int
sumList xs = foldr (+) 0 xs

-- 問題2
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x:myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
  | f x = x:myFilter f xs
  | otherwise = myFilter f xs

oddPlus1 :: [Int] -> [Int]
oddPlus1 xs =
  myMap (+1) [x | x <- xs, x `mod` 2 == 1]

-- 問題3
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys

-- 問題4
-- 想定解答？
split :: [Int] -> ([Int], [Int])
split [] = ([],[])
split [x] = ([x], [])
split (x:y:zs) = (x:xs, y:ys) where (xs, ys) = split zs
-- split (x:y:zs) = (x:first zs, y:second zs)

-- 畳み込みを使った場合
split2 :: [Int] -> ([Int], [Int])
split2 = snd . foldr halve (False, ([], []))
  -- where
  --   halve e (False, (x, y)) = (True, (e:x, y))
  --   halve e (True, (x, y)) = (False, (x, e:y))
halve :: Int -> (Bool, ([Int], [Int])) -> (Bool, ([Int], [Int]))
halve e (False, (x, y)) = (True, (e:x, y))
halve e (True, (x, y)) = (False, (x, e:y))

-- 単に「リストの長さの中間地点」で半分にする場合
split3 :: [a] -> ([a], [a])
split3 [] = ([], [])
split3 xs = splitAt center xs
  where center = (length xs) `div` 2

-- 問題5
msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where (left, right) = split xs
-- msort xs = merge (msort (first xs)) (msort (second xs))

first :: [Int] -> [Int]
first xs = fst (split xs)
second :: [Int]-> [Int]
second xs = snd (split xs)


-- work_3
-- Exercise 1
data Tree =  Leaf | Node Tree Int Tree
preorder :: Tree -> [Int]
preorder Leaf = []
preorder (Node l x r) =
  [x] ++ preorder l ++ preorder r

-- Exercise 2-1
myLookUp :: (Eq a) => a -> [(a, b)] -> Maybe b
-- Exercise 2-2
myLookUp _ [] = Nothing
myLookUp x ((y, z) : yzs)
  | x == y = Just (z)
  | otherwise = myLookUp x yzs
-- myLookUp x (y:ys)
--   | x == fst y = Just (snd y)
--   | otherwise = myLookUp x ys

-- 問題2
postorder :: Tree -> [Int]
postorder Leaf = []
postorder (Node l x r) =
  postorder l ++ postorder r ++ [x]

-- 問題3
nub :: Eq a => [a] -> [a]
nub =  myNubBy (/=)

myNubBy :: (a -> a -> Bool) -> [a] -> [a]
myNubBy _ [] =  []
myNubBy notEq (x : xs) =
  x : myNubBy notEq [y | y <- xs, notEq x y]
-- myNubBy :: (a -> a -> Bool) -> [a] -> [a]
-- myNubBy _ [] =  []
-- myNubBy notEq (x:xs) =
--   x:myNubBy notEq (filter (\y -> notEq x y) xs)


-- work_4
myProduct :: [Int] -> Int
myProduct [] = 0
myProduct [x] = x
myProduct (x : xs) = x * myProduct xs

rev :: [Int] -> [Int]
rev [] = []
rev (x : xs) = rev xs ++[x]

revapp :: [Int] -> [Int] -> [Int]
revapp [] ys = ys
revapp (x : xs) ys = revapp xs (x : ys)


-- キラー数独

-- サンプル
data Exp1 = Val1 Int | Plus1 Exp1 Exp1 | Times Exp1 Exp1
instance Show Exp1 where
    show (Val1 n)       = show n
    show (Plus1 e1 e2)  = "(+ " ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Times e1 e2) = "(* " ++ show e1 ++ " " ++ show e2 ++ ")"

-- 論理式の実装
data Exp = Var Int Int | Val Int | Plus [Exp]

data Formula = And [Formula]
　　　　　　　　| Or [Formula]
　　　　　　　　| Distinct [Exp]
　　　　　　　　| Geq Exp Exp
　　　　　　　　| Eq Exp Exp

showEs :: [Exp] -> String
showEs [] = []
showEs [x] = show x
showEs (x : xs) = show x ++ " " ++ showEs xs

showFs :: [Formula] -> String
showFs [] = []
showFs [f] = show f
showFs (f : fs) = show f ++ " " ++ showFs fs

instance Show Exp where
  show (Val n) = show n
  show (Var i j) = "x" ++ show i ++ show j
  show (Plus []) = []
  show (Plus (e : es)) = "(+ " ++ show e ++ xs ++ ")"
    where xs = intercalate " " [showEs es]
  -- show (Plus (e : es)) = "(+ "  ++ show e ++ " " ++ showEs es ++ ")"

instance Show Formula where
  show (And []) = []
  show (And (f : fs)) = "(and " ++ show f ++ " " ++ showFs fs ++ ")"
  show (Or []) = []
  show (Or (f : fs)) = "(or " ++ show f ++ " " ++ showFs fs ++ ")"
  show (Distinct []) = []
  show (Distinct (e : es)) = "(distinct " ++ show e ++ " " ++ showEs es ++ ")"
  show (Geq e1 e2) = "(>= " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Eq e1 e2) = "(= " ++ show e1 ++ " " ++ show e2 ++ ")"

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

-- 各行に被りがない(今回はi=1で正しく作動)
uniqueRow :: Int -> [Formula]
uniqueRow i
  | i == 9 = [Distinct (row 9 1)]
  | otherwise = [Distinct (row i 1)] ++ uniqueRow (i+1)

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


-- 駄作
toFs :: [Formula]
toFs = [Distinct es | es <- foo]

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