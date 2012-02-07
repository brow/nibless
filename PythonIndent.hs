module PythonIndent
  ( IndentParser
  , indentation
  , indented
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
    GT -> fail "unexpected indent"
    LT -> fail "unexpected unindent"

indented :: IndentParser st a -> IndentParser st a
indented p = do
  indent
  x <- p
  dedent
  return x

indent :: IndentParser st ()
indent = do
  indentLevelAhead <- lookIndentLevel
  indentLevel <- getIndentLevel
  if indentLevelAhead > indentLevel
  then 
    pushIndentLevel indentLevelAhead
  else
    fail "expected an indent" 

dedent :: IndentParser st ()
dedent = do
  indentLevelAhead <- lookIndentLevel
  indentLevel <- getIndentLevel
  if indentLevelAhead < indentLevel
  then 
    void popIndentLevel
  else do
    fail "expected an unindent" 

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
indentUnits = fmap (foldl' addUnits 0) (many (oneOf " \t"))
  where addUnits n ' '  = n + 1
        addUnits n '\t' = roundUp 8 (n + 1)

lookIndentLevel :: IndentParser st Int 
lookIndentLevel = lookAhead (many (try emptyLine) >> indentUnits) 

emptyLine :: IndentParser st ()
emptyLine = many (oneOf " \t") >> void (char '\n')
