import Text.ParserCombinators.Parsec

-- (SMTOutput は Z3 の出力結果のこと)
-- 読み取るのは "sat ((x11 2) (x12 1) ... (x99 9))"
-- 返すのが Just ([("x11", 2), ("x12", 1), ..., ("x99", 9)]) or Nothing

-- Scanners

keyword :: String -> Parser ()
keyword s = do
  spaces
  _ <- string s
  spaces

parseVar :: Parser String
parseVar = do
  spaces
  s <- many1 (noneOf " \n\r\t\0,()")
  spaces
  return s

-- Parsers

parseSample :: Parser (String, [String])
parseSample = do
  keyword "begin"
  s <- parseVar
  keyword "("
  ss <- many parseVar
  keyword ")"
  keyword "end"
  return (s, ss)

-- test
-- parse parseSMTOutput "file" "sat ((x11 1) (x12 2))"

-- >>> parseTest parseSample "begin abc() end"
-- >>> parseTest parseSample "begin abc(cd) end"
-- >>> parseTest parseSample "begin abc(cd e f) end"
-- ("abc",[])
-- ("abc",["cd"])
-- ("abc",["cd","e","f"])
--

-- >>> parse parseSample "filename" "begin abc(cd e f) end"
-- 　　　　　　　　　　　　　　　　　　　　ここにz3の実行結果が入る？
-- >>> parse parseSample "/z3" "begin abc cd e f) end"
-- Right ("abc",["cd","e","f"])
-- Left "filename" (line 1, column 11):
-- unexpected "c"
-- expecting space, white space or "("
--
-- begin abc(cd e f)
-- abc(cd e f)
-- abc(cd e f) end