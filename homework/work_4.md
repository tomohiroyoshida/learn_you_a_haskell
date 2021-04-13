# Work_4

### Definitions

```
A1 : [] ++ ys = ys
A2 : (x : xs) ++ ys = x : (xs ++ ys)
R1 : rev [] = []
R2 : rev (x : xs) = rev xs ++[x]
R3 : revapp [] ys = ys
R4 : revapp (x : xs) ys = revapp xs (x : ys)
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

- If xs = ys = zs = [] then

  [] ++ ([] ++ [])  
  = [] ++ [] &emsp;&emsp;&emsp;by A1  
  = [] ++ [] ++ [] by A1  
  = ([] ++ []) ++ []

- If xs = (x:xs'), ys = (y:ys') and zs = (z:zs') then

  (x : xs') ++ ((y : ys') ++ (z : zs'))  
  = x : (xs' ++ ((y : ys') ++ (z : zs'))) by A2  
  = x : ((xs' ++ (y : ys')) ++ (z:zs')) &nbsp;&nbsp;by I.H.  
  = (x : (xs' ++ (y : ys'))) ++ (z:zs') &nbsp;&nbsp;by A2  
  = ((x : xs') ++ (y : ys')) ++ (z:zs') &nbsp;&nbsp;by A2  
  = (xs ++ ys) ++ zs

### No.3

```
Consider the two recursive functions on lists:
rev [] = []
rev (x : xs) = rev xs ++[x]
revapp [] ys = ys
revapp (x : xs) ys = revapp xs (x : ys)
Show rev xs = revapp xs [] for all lists xs.
```

We show the claim by structural induction on xs.

- If xs = [] then

  rev [] = [] &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;by R1  
  revapp [] [] = [] by R3

- If xs = (x : xs') then  
  rev (x : xs')  
  = rev xs

  revapp (x : xs') []  
  = revapp xs' (x : []) by R4  
  = revapp xs' [x]  
  = rev xs' ++ [x] &nbsp;&nbsp;&nbsp;&nbsp; by lemma  
  = rev (x : xs') &emsp;&emsp;&nbsp;by R4  
  = rev xs

  lemma:  
   revapp xs' [x] = rev xs' ++ [x]
