myGcd :: Integer -> Integer -> Integer
myGcd x 0 = x
myGcd x y =
  if x < y then myGcd y x
  else myGcd y (x `mod` y)

myRange :: Integer -> Integer -> [Integer]
myRange x y = [x..y]

myInsert :: Integer -> [Integer] -> [Integer]
myInsert x [] = [x]
myInsert x (y:ys) =
  if x <= y then x:y:ys
  else  y:myInsert x ys

myIsort :: [Integer] -> [Integer]
myIsort [] = []
myIsort (x:xs) = myInsert x (myIsort xs)