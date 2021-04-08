-- 問題5
sumList :: [Int] -> Int
sumList [] = 0
sumList xs = foldr (+) 0 xs

-- 問題6
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x:myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
  | p x == False = myFilter p xs
  | otherwise = x:myFilter p xs

oddPlus1 :: [Int] -> [Int]
oddPlus1 [] = []
oddPlus1 xs =
  myMap (+ 1) (myFilter (\x -> mod x 2 == 1) xs)

-- 問題7
merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge xs ys = xs ++ ys

-- 問題8
split :: [a] -> ([a], [a])
split = snd . foldr go (False, ([], []))
  where
    go e (False, (a, b)) = (True, (e:a, b))
    go e (True, (a, b)) = (False, (a, e:b))

split2 :: [a] -> ([a], [a])
split2 [] = ([], [])
split2 xs = splitAt center xs
  where center = (length xs) `div` 2

-- 問題9
msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where (left, right) = split xs