import PythonIndent
import PythonIndent.Prim (parseFromFile)
import Text.Parsec
import Control.Monad
import Data.Maybe
import Text.Show.Pretty (ppShow)

data Tree = Node String [Tree] deriving Show

main = parseFromFile tree "file.txt" >>= putStrLn . ppShow

tree :: IndentParser st Tree
tree = do
  str <- many (noneOf "\n")
  newline
  children <- optionMaybe (try (manyIndented tree))
  return (Node str (fromMaybe [] children))
