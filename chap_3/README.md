# Types and Typeclasses

### Believe the type

- Everything in Haskell has a type, so the compiler can reason quite a lot about your program before compiling it.

- Note that the empty tuple `()` is also a type which can only have a single value: `()`

- **type variable**

```
ghci> :t head
head :: [a] -> a
```

That means that `a` can be of any type.
This is much like generics in other languages, only in Haskell it's much more powerful because it allows us to easily write very general functions if they don't use any specific behavior of the types in them.
Functions that have type variables are called **polymorphic functions**.

### Typeclasses 101

```
ghci> :t (==)
(==) :: (Eq a) => a -> a -> Bool
```

- Everything before the `=>` symbol is called a **class constraint**.
  We can read the previous type declaration like this: the equality function takes any two values that are of the same type and returns a `Bool`.
  The type of those two values must be a member of the Eq class (this was the class constraint).
