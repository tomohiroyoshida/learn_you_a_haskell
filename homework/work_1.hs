-- 問題1
myGcd :: Integer -> Integer -> Integer
myGcd x 0 = x
myGcd x y
  | x < y = myGcd y x
  | otherwise = myGcd y (x `mod` y)

-- 問題2
myRange :: Integer -> Integer -> [Integer]
myRange x y
  | x > y = []
  | x == y = [x]
  | otherwise = x:myRange (x+1) y

-- 問題3
myInsert :: Integer -> [Integer] -> [Integer]
myInsert x [] = [x]
myInsert x (y:ys)
  | x <= y = x:y:ys
  | otherwise = y:myInsert x ys

-- 問題4
myIsort :: [Integer] -> [Integer]
myIsort [] = []
myIsort (x:xs) = myInsert x (myIsort xs)