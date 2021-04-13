myProduct :: [Int] -> Int
myProduct [] = 0
myProduct [x] = x
myProduct (x : xs) = x * myProduct xs

-- Definitions
-- A1 : [ ] ++ ys = ys
-- A2 : (x : xs) ++ ys = x : (xs ++ ys)
-- L1 : length [] = 0
-- L2 : length (x : xs) = 1 + length xs
-- R1 : rev [] = []
-- R2 : rev (x : xs) = rev xs ++[x]
-- R3 : revapp [] ys = ys
-- R4 : revapp (x : xs) ys = revapp xs (x : ys)

rev :: [Int] -> [Int]
rev [] = []
rev (x : xs) = rev xs ++[x]

revapp :: [Int] -> [Int] -> [Int]
revapp [] ys = ys
revapp (x : xs) ys = revapp xs (x : ys)