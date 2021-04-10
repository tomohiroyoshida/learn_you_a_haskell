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
split :: [a] -> ([a], [a])
split = snd . foldr halve (False, ([], []))
  where
    halve e (False, (x, y)) = (True, (e:x, y))
    halve e (True, (x, y)) = (False, (x, e:y))

-- 単に「リストの長さの中間地点」で半分にする場合
split2 :: [a] -> ([a], [a])
split2 [] = ([], [])
split2 xs = splitAt center xs
  where center = (length xs) `div` 2

-- 問題5
msort :: [Int] -> [Int]
msort xs = merge (first xs) (second xs)

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
myLookUp :: Int -> [(Int, String)] -> Maybe String
-- Exercise 2-2
myLookUp _ [] = Nothing
myLookUp x (y:ys)
  | x == fst y = Just (snd y)
  | otherwise = myLookUp x ys

-- 問題2
postorder :: Tree -> [Int]
postorder Leaf = []
postorder (Node l x r) =
  postorder l ++ postorder r ++ [x]

-- 問題3
nub :: (Eq a) => [a] -> [a]
nub =  nubBy (/=)

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy _ [] =  []
nubBy notEq (x:xs) =
  x:nubBy notEq (filter (\y -> notEq x y) xs)