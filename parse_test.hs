import Pydent
import Pydent.Prim (parseFromFile)
import Text.Parsec
import Control.Monad
import Data.Maybe
import Text.Show.Pretty (ppShow)

data Tree = Node String [Tree] deriving Show

main = parseFromFile file "file.txt" >>= putStrLn . ppShow

file :: PydentParser st [Tree]
file = do
  xs <- many (try tree)
  return xs

tree :: PydentParser st Tree
tree = do
  skipMany (try emptyLine)
  str <- many1 (noneOf "\n")
  newline
  children <- optionMaybe (try (manyIndented tree))
  return (Node str (fromMaybe [] children))
