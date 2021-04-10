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
split = snd . foldr halve (False, ([], []))

halve :: Int -> (Bool, ([Int], [Int]))
        -> (Bool, ([Int], [Int]))
halve e (False, (x, y)) = (True, (e:x, y))
halve e (True, (x, y)) = (False, (x, e:y))

-- split [] = ([],[])
-- split [x] = ([x], [])
-- split (x:y:zs) = ???

-- 問題5
msort :: [Int] -> [Int]
msort xs = merge (first xs) (second xs)

first :: [Int] -> [Int]
first xs = fst (split xs)
second :: [Int]-> [Int]
second xs = snd (split xs)