### 2. Starting Out

##### Baby's first functions

- `+` expects its left and right side to be numbers.
- If we tried to do True == 5, GHCI would tell us that the types don't match. Whereas + works only on things that are considered numbers, `==` works on any two things that can be compared.
- But the catch is that they both have to be the same type of thing.

- you can do 5 + 4.0 because 5 is sneaky and can act like an integer or a floating-point number. 4.0 can't act like an integer, so 5 is the one that has to adapt.

- The else part is mandatory in Haskell. In Haskell every expression and function must return something.
- Because the else is mandatory, an if statement will always return something and that's why it's an expression.
- If we wanted to add one to every number that's produced in our previous function, we could have written its body like this.

```
  doubleSmallNumber' x = (if x > 100 then x else x\*2) + 1
```

```
  conanO'Brien = "It's a-me, Conan O'Brien!"
```

- In the function name we didn't capitalize Conan's name.
- That's because functions can't begin with uppercase letters. We'll see why a bit later.
- The second thing is that this function doesn't take any parameters. When a function doesn't take any parameters, we usually say it's a definition (or a name).

##### An intro to lists

- In Haskell, lists are a **homogenous** data structure.
- That means that we can have a list of integers or a list of characters but we can't have a list that has a few integers and then a few characters.
- Strings are just lists of characters. `"hello"` is just syntactic sugar for `['h','e','l','l','o']`.
  Because strings are lists, we can use list functions on them, which is really handy.

- A common task is putting two lists together. This is done by using the ++ operator.

```

ghci> [1,2,3,4] ++ [9,10,11,12]
[1,2,3,4,9,10,11,12]
ghci> "hello" ++ " " ++ "world"
"hello world"
ghci> ['w','o'] ++ ['o','t']
"woot"

```

- Putting something at the beginning of a list using the : operator (also called the cons operator) is instantaneous.

```
ghci> 'A':" SMALL CAT"
"A SMALL CAT"
ghci> 5:[1,2,3,4,5]
[5,1,2,3,4,5]
```

- If you want to get an element out of a list by index, use `!!`. The indices start at 0.

```
ghci> "Steve Buscemi" !! 6
'B'
ghci> [9.4,33.2,96.2,11.2,23.25] !! 1
33.2
```

- `product [] == 1`

##### Texas ranges

```
ghci> [1..20]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
ghci> ['a'..'z']
"abcdefghijklmnopqrstuvwxyz"
ghci> ['K'..'Z']
"KLMNOPQRSTUVWXYZ"
```

##### I'm a list comprehension

```
ghci> [x*2 | x <- [1..10]]
[2,4,6,8,10,12,14,16,18,20]
```

- The list comprehension we could use is [x\*2 | x <- [1..10]]. x is drawn from [1..10] and for every element in [1..10] (which we have bound to x), we get that element, only doubled.
- Predicates go after the binding parts and are separated from them by a comma.
  Let's say we want only the elements which, doubled, are greater than or equal to 12.

```
ghci> [x*2 | x <- [1..10], x*2 >= 12]
[12,14,16,18,20]
```

- Not only can we have multiple predicates in list comprehensions (an element must satisfy all the predicates to be included in the resulting list), we can also draw from several lists. When drawing from several lists, comprehensions produce all combinations of the given lists and then join them by the output function we supply.

```
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]
[16,20,22,40,50,55,80,100,110]
```

- Nested list comprehensions are also possible if you're operating on lists that contain lists. A list contains several lists of numbers. Let's remove all odd numbers without flattening the list.

```
ghci> let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
ghci> [ [ x | x <- xs, even x ] | xs <- xxs]
[[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]
```

##### Tuples

- Tuples, however, are used when you know exactly how many values you want to combine and its type depends on how many components it has and the types of the components. They are denoted with parentheses and their components are separated by commas.
- Another key difference is that they don't have to be homogenous. Unlike a list, a tuple can contain a combination of several types.

- Use tuples when you know in advance how many components some piece of data should have.
  Tuples are much more rigid because each different size of tuple is its own type, so you can't write a general function to append an element to a tuple — you'd have to write a function for appending to a pair, one function for appending to a triple, one function for appending to a 4-tuple, etc.

- What happens if the lengths of the lists don't match?

```
ghci> zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]
[(5,"im"),(3,"a"),(2,"turtle")]
```

- The longer list simply gets cut off to match the length of the shorter one. Because Haskell is lazy, we can zip finite lists with infinite lists:

```
ghci> zip [1..] ["apple", "orange", "cherry", "mango"]
[(1,"apple"),(2,"orange"),(3,"cherry"),(4,"mango")]
```
