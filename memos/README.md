# Memos

- `x:xs` について
  (x:xs)で先頭 `x` とその後ろのリスト `xs` に分割して受け取ります。`xs` は `x` の複数形を意図しています。これらの名前には文法的な意味はなく、あくまで慣習です。

  ```
  first (x:xs) = x

  main = do
  print $ first [1..5]
  print $ first "abcdef"
  ```

  実行結果

  ```
  1
  'a'
  ```

  [ソース資料](https://qiita.com/7shi/items/145f1234f8ec2af923ef#引数)

- 合成関数 `.`
  `.` を使うことで関数同士を合成することができる。
  一番右の関数から評価される。

  ```
  foo x = f (g (h x))
  foo x = (f . g . h) x
  foo = f . g . h
  ```

  [ソース資料](http://walk.northcol.org/haskell/functions/#_%E9%96%A2%E6%95%B0%E5%90%88%E6%88%90)
