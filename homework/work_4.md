# Work_4

### Definitions

```
A1 : [] ++ ys = ys
A2 : (x : xs) ++ ys = x : (xs ++ ys)
A3 : xs ++ [] = xs
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

- If xs = (x : xs') then

  (x : xs') ++ (ys ++ zs)  
  = x : (xs' ++ (ys ++ zs)) &nbsp;&nbsp;by A2  
  = x : ((xs' ++ ys) ++ zs) &nbsp;&nbsp;by I.H.  
  = (x : (xs' ++ ys)) ++ zs &nbsp;&nbsp;by A2  
  = ((x : xs') ++ ys) ++ zs &nbsp;&nbsp;by A2  
  = (xs ++ ys) ++ zs

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

  rev [] = [] &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;by R1  
  revapp [] [] = [] by R3

- If xs = (x : xs') then  
  rev (x : xs')  
  = rev xs

  revapp (x : xs') []  
  = revapp xs' (x : []) by R4  
  = revapp xs' [x]  
  = rev xs' ++ [x] &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; by lemma  
  = rev (x : xs') &emsp;&emsp;&nbsp; by R2  
  = rev xs

- lemma

  ```
  revapp xs ys = rev xs ++ ys
  ```

  We show the claim by structural induction on xs and ys.

  - If xs = [] then  
    revapp [] ys = ys by R3
    rev [] ++ ys = ys by R1

  - If ys = [] then  
    revapp (x : xs') []  
    = revapp xs' (x : []) by R4  
    = ???

    rev xs ++ []  
    = rev xs by R1

  - If xs = (x : xs'), (y: ys') then  
    revapp (x : xs') (y : ys')  
    = revapp xs' (x : y : ys') by R4  
    = rev xs' ++ (x : y : ys') by I.H.  
    = rev (x : y : ys') : xs' by R2  
    = ???

<!-- No. 1 は帰納法の仮定 (induction hypothesis, I.H.) をどこで
用いていますか？わかるように記して下さい。

No. 2 は場合分けを取り尽くしていません。 xs = [1], ys = zs = [] の
とき、証明が対応できません。そもそも本当に場合分けしないといけない
変数はどれなのでしょうか。証明を整理して下さい。

No. 3 は補題の証明を書いて下さい。また帰納法の仮定をどこで
用いていますか？わかるように記して下さい。 -->
