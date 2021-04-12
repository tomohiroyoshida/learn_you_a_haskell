# Work_4

### Definitions

```
A1 : [] ++ ys = ys
A2 : (x : xs) ++ ys = x : (xs ++ ys)
L1 : length [] = 0
L2 : length (x : xs) = 1 + length xs
R1 : rev [] = []
R2 : rev (x : xs) = rev xs ++[x]
```

### No.1

```
Show that
xs ++ [] = xs
holds for all lists xs.
```

We show the claim by structural induction on xs.

- If xs = [] then

  [] ++ [] = []

- If xs = x : xs' then

  x : xs' ++ []  
  = x : (xs' ++ []) by A1  
  = x : xs'  
  = xs

### No.2

```
Show that
xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
holds for all lists xs, ys, zs
```

We show the claim by structural induction on xs, ys and zs.

- If xs == ys == zs == [] then

  [] ++ ([] ++ [])  
  = [] + [] &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; by A1  
  = [] ++ [] ++ [] by A1  
  = ([] ++ []) ++ []

- If xs = (x:xs'), ys = (y:ys') and zs = (z:zs') then

  (x : xs') ++ ((y : ys') ++ (z : zs'))  
  = x : (xs' ++ ((y : ys') ++ (z : zs'))) by A2  
  = x : ((xs' ++ (y : ys')) ++ (z:zs')) by I.H.  
  = (x : (xs' ++ (y : ys'))) ++ (z:zs') by A2  
  = ((x : xs') ++ (y : ys')) ++ (z:zs') by A2  
  = (xs ++ ys) ++ zs
