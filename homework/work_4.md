# Work_4

### Definitions

```
A1 : [] ++ ys = ys
A2 : (x : xs) ++ ys = x : (xs ++ ys)
(A3 : xs ++ [] = xs)
(A4 : xs ++ (ys ++ zs) = (xs ++ ys) ++ zs)
R1 : rev [] = []
R2 : rev (x : xs) = rev xs ++ [x]
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

  [] ++ [] = [] by A1

- If xs = x : xs' then

  (x : xs') ++ []  
  = x : (xs' ++ []) by A2  
  = x : xs' &emsp;&emsp;&emsp; by I.H.  
  = xs

Now we use the claim as the definition of `A3`

### No.2

```
Show that
xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
holds for all lists xs, ys, zs
```

We show the claim by structural induction on xs.

- If xs = [] then

  [] ++ (ys ++ zs)  
  = ys ++ zs &emsp;&emsp;&nbsp; by A1

  ([] ++ ys) ++ zs  
  = ys ++ zs &emsp;&emsp;&nbsp; by A1

- If xs = (x : xs') then

  (x : xs') ++ (ys ++ zs)  
  = x : (xs' ++ (ys ++ zs)) &nbsp;&nbsp;by A2  
  = x : ((xs' ++ ys) ++ zs) &nbsp;&nbsp;by I.H.  
  = (x : (xs' ++ ys)) ++ zs &nbsp;&nbsp;by A2  
  = ((x : xs') ++ ys) ++ zs &nbsp;&nbsp;by A2  
  = (xs ++ ys) ++ zs

Now we can use the claim as the definition of `A4`.

### No.3

```
Consider the two recursive functions on lists:
rev [] = []
rev (x : xs) = rev xs ++ [x]
revapp [] ys = ys
revapp (x : xs) ys = revapp xs (x : ys)

Show rev xs = revapp xs [] for all lists xs.
```

We show the claim by structural induction on xs.

- If xs = [] then

  rev [] = [] &nbsp;&emsp;&emsp;&nbsp; by R1

  revapp [] [] = [] by R3

- If xs = (x : xs') then

  rev (x : xs')  
  = rev xs

  revapp (x : xs') []  
  = revapp xs' (x : []) &nbsp; by R4  
  = revapp xs' ([x] ++ []) by lemma1  
  = revapp xs' [x] &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; by A3  
  = rev xs' ++ [x] &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; by lemma2  
  = rev (x : xs') &nbsp;&emsp;&emsp;&nbsp; by R2  
  = rev xs

---

- lemma1

  ```
  x : ys = [x] ++ ys
  ```

  We show the claim by structural induction on ys.

  - If ys = [] then

    x : []  
    = [x] &emsp;&emsp;&emsp; by A3

    [x] ++ []  
    = [x] &emsp;&emsp;&emsp; by A3

  - If ys = (y : ys') then

    x : (y : ys')  
    = [x] ++ (y : ys') &emsp;&emsp; by I.H.  
    = [x] ++ ([y] ++ ys') &nbsp; by I.H.  
    = [x] ++ ys

- lemma2

  ```
  revapp xs ys = rev xs ++ ys
  ```

  We show the claim by structural induction on xs.

  - If xs = [] then

    revapp [] ys = ys &nbsp;&nbsp; by R3

    rev [] ++ ys = ys &nbsp;&nbsp;&nbsp; by R1

  - If xs = (x : xs') then

    revapp (x : xs') ys  
    = revapp xs' (x : ys) &nbsp;by R4  
    = rev xs' ++ (x : ys) &nbsp; by I.H.  
    = rev xs' ++ ([x] ++ ys) &nbsp; by lemma1  
    = (rev xs' ++ [x]) ++ ys &nbsp; by A4  
    = rev (x : xs') ++ ys &nbsp; by R2  
    = rev xs ys

---
