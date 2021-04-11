-- 問題1
sumList :: [Int] -> Int
sumList xs = foldr (+) 0 xs

-- 問題2
oddPlus1 :: [Int] -> [Int]
oddPlus1 xs =
  map (+1) [x | x <- xs, x `mod` 2 == 1]

-- 問題3
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys

-- 問題4
split :: [Int] -> ([Int], [Int])
split [] = ([],[])
split [x] = ([x], [])
split (x:y:zs) = (x:first zs, y:second zs)

first :: [Int] -> [Int]
first xs = fst (split xs)
second :: [Int]-> [Int]
second xs = snd (split xs)

-- 問題5
msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where (left, right) = split xs
-- msort xs = merge (msort (first xs)) (msort (second xs))