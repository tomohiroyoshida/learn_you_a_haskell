doubleMe x = x + x
-- doubleUs x y = x*2 + y*2
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x =
  (if x < 100
  then x
  else x*2) + 1

boombang xs = [ if x < 10  then "BOOM!" else "BANG!" | x <- xs, odd x]

notTen xs = [ x | x <- xs, x/= 10]

multiList xs ys = [x + y | x <- xs, y <- ys]

multiListWithMin xs ys = [x + y | x <- xs, y <- ys, x + y > 3]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

rightTriangles cs bs as  =
  [ (a,b,c) | c <- cs, b <- bs, a <- as, a < b, b < c, a^2 + b^2 == c^2]