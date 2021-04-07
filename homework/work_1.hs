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