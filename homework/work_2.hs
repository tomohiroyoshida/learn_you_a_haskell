-- 問題5
sumList :: [Int] -> Int
sumList [] = 0
sumList xs = foldr (+) 0 xs

-- 問題6
oddPlus1 :: [Int] -> [Int]
oddPlus1 [] = []
oddPlus1 xs =
  map (+1) [x | x <- xs, x `mod` 2 == 1]

-- 問題7
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys

-- 問題8
split :: [Int] -> ([Int], [Int])
split = snd . foldr halve (False, ([], []))
  where
    halve e (False, (x, y)) = (True, (e:x, y))
    halve e (True, (x, y)) = (False, (x, e:y))

-- 問題9
msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where (left, right) = split xs