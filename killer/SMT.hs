import Text.ParserCombinators.Parsec
import System.Process

-- 論理式の実装
data Exp = Var Int Int | Val Int | Plus [Exp]

data Formula = And [Formula]
              | Or [Formula]
              | Distinct [Exp]
              | Geq Exp Exp
              | Eq Exp Exp

instance Show Exp where
  show (Val n) = show n
  show (Var i j) = "x" ++ show i ++ show j
  show (Plus []) = []
  show (Plus (e : es)) = "(+ " ++ show e ++ " " ++ showEFs es ++ ")"

instance Show Formula where
  show (And []) = "true"
  show (And (f : fs)) = "(and " ++ show f ++ " " ++ showEFs fs ++ ")"
  show (Or []) = "false"
  show (Or (f : fs)) = "(or " ++ show f ++ " " ++ showEFs fs ++ ")"
  show (Distinct []) = ""
  show (Distinct (e : es)) = "(distinct " ++ show e ++ " " ++ showEFs es ++ ")"
  show (Geq e1 e2) = "(>= " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Eq e1 e2) = "(= " ++ show e1 ++ " " ++ show e2 ++ ")"

showEFs :: Show a => [a] -> String
showEFs es = unwords [ show e | e <- es]

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