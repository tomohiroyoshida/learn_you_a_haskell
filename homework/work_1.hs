-- 問題1
myGcd :: Integer -> Integer -> Integer
myGcd x 0 = x
myGcd x y =
  if x < y then myGcd y x
  else myGcd y (x `mod` y)

-- 問題2
myRange :: Integer -> Integer -> [Integer]
myRange x y =
  if x > y then []
  else if x == y then [x]
  else x:myRange (x+1) y

-- 問題3
myInsert :: Integer -> [Integer] -> [Integer]
myInsert x [] = [x]
myInsert x (y:ys) =
  if x <= y then x:y:ys
  else  y:myInsert x ys

-- 問題4
myIsort :: [Integer] -> [Integer]
myIsort [] = []
myIsort (x:xs) = myInsert x (myIsort xs)