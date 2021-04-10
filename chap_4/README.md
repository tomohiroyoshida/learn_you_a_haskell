# Syntax in Functions

### Pattern matching

- The `x:xs` pattern is used a lot, especially with recursive functions. But patterns that have `:` in them only match against lists of length 1 or more.

```
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
te@ll (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y
```

- This function is safe because it takes care of the empty list, a singleton list, a list with two elements and a list with more than two elements. Note that `(x:[])` and `(x:y:[])` could be rewriten as `[x]` and `[x,y]` (because its syntatic sugar, we don't need the parentheses).
- We can't rewrite (x:y:\_) with square brackets because it matches any list of length 2 or more.

- There's also a thing called as patterns. Those are a handy way of breaking something up according to a pattern and binding it to names whilst still keeping a reference to the whole thing.
  You do that by putting a name and an `@` in front of a pattern. For instance, the pattern `xs@(x:y:ys)`. This pattern will match exactly the same thing as `x:y:ys` but you can easily get the whole list via xs instead of repeating yourself by typing out `x:y:ys` in the function body again.
  Normally we use as patterns to avoid repeating ourselves when matching against a bigger pattern when we have to use the whole thing again in the function body.
  Here's a quick and dirty example:

```
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
ghci> capital "Dracula"
"The first letter of Dracula is D"
```

- you can't use `++` in pattern matches. If you tried to pattern match against `(xs ++ ys)`, what would be in the first and what would be in the second list? It doesn't make much sense. It would make sense to match stuff against `(xs ++ [x,y,z])` or just `(xs ++ [x])`, but because of the nature of lists, you can't do that.

### Guards, guards!

```
max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b
### get around them
```

### Where

```
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0
```

- The names we define in the where section of a function are only visible to that function, so we don't have to worry about them polluting the namespace of other functions.
  Notice that all the names are aligned at a single column. If we don't align them nice and proper, Haskell gets confused because then it doesn't know they're all part of the same block.
  `where` bindings aren't shared across function bodies of different patterns. If you want several patterns of one function to access some shared name, you have to define it globally.

```
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2
```

- `where` bindings can also be nested. It's a common idiom to make a function and define some helper function in its where clause and then to give those functions helper functions as well, each with its own where clause.

### Let it be

- `Where` bindings are a syntactic construct that let you bind to variables at the end of a function and the whole function can see them, including all the guards.
- `Let` bindings let you bind to variables anywhere and are expressions themselves, but are very local, so they don't span across guards. Just like any construct in Haskell that is used to bind values to names, let bindings can be used for pattern matching.

- ```
  cylinder :: (RealFloat a) => a -> a -> a
  cylinder r h =
      let sideArea = 2 * pi * r * h
          topArea = pi * r ^2
      in  sideArea + 2 * topArea
  let it be
  ```

- The form is `let <bindings> in <expression>`. The names that you define in the let part are accessible to the expression after the in part. As you can see, we could have also defined this with a where binding. Notice that the names are also aligned in a single column. So what's the difference between the two? For now it just seems that let puts the bindings first and the expression that uses them later whereas where is the other way around.

```
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
```

- We can't use the bmi name in the (w, h) <- xs part because it's defined prior to the let binding.

### Case expressions

```
head' :: [a] -> a
head' [] = error "No head for empty lists!"
head' (x:_) = x
head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x  S
```

- Whereas pattern matching on function parameters can only be done when defining functions, case expressions can be used pretty much anywhere. For instance:

```
describeList :: [a] -> String
describeList xs =
"The list is " ++ case xs of [] -> "empty."
                            [x] -> "a singleton list."
                            xs -> "a longer list."
```
