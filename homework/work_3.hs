data Tree =  Leaf | Node Tree Int Tree

-- Exercise
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