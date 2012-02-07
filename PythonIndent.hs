module PythonIndent
  ( IndentParser
  , indentation
  , indented
  , manyIndented
  , emptyLine ) where

import PythonIndent.Prim
import Data.List (foldl')
import Control.Monad
import Text.ParserCombinators.Parsec

indentation :: IndentParser st ()
indentation = do
  consumed <- indentUnits
  indentLevel <- getIndentLevel
  case compare consumed indentLevel of
    EQ -> return ()
    GT -> unexpected "indent"
    LT -> unexpected "unindent"

indented :: IndentParser st a -> IndentParser st a
indented p = do
  indent
  x <- p
  dedent 
  return x

manyIndented :: IndentParser st a -> IndentParser st [a]
manyIndented p = do
  indent
  manyTill (skipEmptyLines >> indentation >> p) (try dedent)
  -- why is `try` needed above? `dedent` should never consume anything

indent :: IndentParser st ()
indent = do
  indentLevelAhead <- tryLookIndentUnits
  indentLevel <- getIndentLevel
  if indentLevelAhead > indentLevel
  then pushIndentLevel indentLevelAhead
  else expected "indent"

dedent :: IndentParser st ()
dedent = do
  indentLevelAhead <- tryLookIndentUnits
  indentLevel <- getIndentLevel
  if indentLevelAhead < indentLevel
  then void popIndentLevel
  else expected "unindent"

getIndentLevel :: IndentParser st Int
getIndentLevel = getIndentStack >>= return . head

pushIndentLevel :: Int -> IndentParser st ()
pushIndentLevel n = getIndentStack >>= setIndentStack . (n:)

popIndentLevel :: IndentParser st Int
popIndentLevel = do
  indentStack <- getIndentStack
  case indentStack of
    [0] -> fail "tried to pop base indent level"
    (x:xs) -> setIndentStack xs >> return x

roundUp :: Integral a => a -> a -> a
roundUp m n = n + mod (-n) m

indentUnits :: IndentParser st Int
indentUnits = fmap (foldl' addUnits 0) (many indentChar)
  where addUnits n '\t' = roundUp 8 (n + 1)
        addUnits n _  = n + 1

tryLookIndentUnits :: IndentParser st Int 
tryLookIndentUnits = (try . lookAhead) (skipEmptyLines >> indentUnits) 

expected :: String -> IndentParser st ()
expected = (void (oneOf []) <?>)

indentChar :: IndentParser st Char
indentChar = oneOf " \t"

emptyLine :: IndentParser st ()
emptyLine = many indentChar >> void newline

skipEmptyLines :: IndentParser s ()
skipEmptyLines = skipMany (try emptyLine) 
