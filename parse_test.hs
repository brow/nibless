import PythonIndent
import PythonIndent.Prim (parseFromFile)
import Text.Parsec
import Control.Monad

main = parseFromFile parser "file.txt" >>= putStrLn . show

line = do
  skipMany (try emptyLine)
  indentation
  x <- many (noneOf "\n")
  newline
  return x

parser = do
  ln1 <- line
  sublines <- indented (many (try line))
  ln2 <- line
  ln3 <- indented line
  return ln3
