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
