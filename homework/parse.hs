import Text.ParserCombinators.Parsec
import System.Process

-- scanners
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

myParse :: Parser (String, Int)
myParse = do
  keyword "("
  s <- many1 (noneOf " \n\r\t\0,()")
  spaces
  n <- many1 (noneOf " \n\r\t\0,()")
  keyword ")"
  return (s,(read n :: Int))

-- SMTOutput
type SMTOutput = Maybe [(String, Int)]

parseSMTOutput :: Parser SMTOutput
parseSMTOutput = try parseSat <|> parseUnsat

parseSat :: Parser SMTOutput
parseSat = do
  keyword "sat"
  keyword "("
  s <- many myParse
  keyword ")"
  return (Just s)

parseUnsat :: Parser SMTOutput
parseUnsat = do
  keyword  "unsat"
  return Nothing

-- solve
type SMTInput = FilePath

solve :: String -> SMTInput -> IO SMTOutput
solve toolPath input = do
  result <- readProcess toolPath [input] []
  return (fromRight Nothing (parse parseSMTOutput "z3result.txt" result))

fromRight :: SMTOutput -> Either ParseError SMTOutput -> SMTOutput
fromRight _ (Right s) = s
fromRight s _ = s