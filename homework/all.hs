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

-- 問題5
-- Use foldr to implement sumList:
-- sumList [x1, . . . , xn] = x1 + · · · + xn

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr k z = go
  where
    go [] = z
    go (y:ys) = y `k` go ys

sumList :: [Int] -> Int
sumList [] = 0
sumList xs = foldr (+) 0 xs

-- 問題6
-- Use list comprehension to re-implement
-- oddplus1: oddplus1 xs =
-- map (+ 1) (filter (\x -> mod x 2 == 1) xs)

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x:myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
  | f x = x:myFilter f xs
  | otherwise = myFilter f xs

oddPlus1 :: [Int] -> [Int]
oddPlus1 [] = []
oddPlus1 xs =
  myMap (+1) [x | x <- xs, x `mod` 2 == 1]

-- 問題7
-- Implement merge :: [Int] → [Int] → [Int].

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys

-- 問題8
-- Implement split :: [Int] → ([Int], [Int]):
-- split [x1,...,xn] = ([x1,x3,...],[x2,x4,...])

split :: [a] -> ([a], [a])
split = snd . foldr halve (False, ([], []))
  where
    halve e (False, (x, y)) = (True, (e:x, y))
    halve e (True, (x, y)) = (False, (x, e:y))

split2 :: [a] -> ([a], [a])
split2 [] = ([], [])
split2 xs = splitAt center xs
  where center = (length xs) `div` 2

-- 問題9
-- Implement msort :: [Int] → [Int].

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where (left, right) = split xs