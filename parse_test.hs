import PythonIndent
import PythonIndent.Prim (parseFromFile)
import Text.Parsec
import Control.Monad

main = parseFromFile parser "file.txt" >>= putStrLn . show

line = do
  indentation
  x <- many (noneOf "\n")
  newline
  return x

parser = do
  ln1 <- line
  sublines <- indented (many line)
  ln4 <- line
  ln5 <- indented line
  return sublines
