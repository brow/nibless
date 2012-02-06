module PythonIndent
  ( IndentParser
  , indentation
  , indented ) where

import PythonIndent.Prim
import Data.List (foldl')
import Control.Monad
import Text.Parsec (oneOf, many, lookAhead)

indentation :: IndentParser st ()
indentation = do
  consumed <- indentUnits
  indentLevel <- getIndentLevel
  case compare consumed indentLevel of
    EQ -> return ()
    LT -> fail "unexpected unindent"
    GT -> fail "unexpected indent"

indented :: IndentParser st a -> IndentParser st a
indented p = do
  indent
  x <- p
  dedent
  return x

indent :: IndentParser st ()
indent = do
  indentLevelAhead <- lookAhead indentUnits
  indentLevel <- getIndentLevel
  if indentLevelAhead > indentLevel
  then 
    pushIndentLevel indentLevelAhead
  else
    fail "expected an indent" 

dedent :: IndentParser st ()
dedent = do
  indentLevelAhead <- lookAhead indentUnits
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

roundUp ::  Integral a => a -> a -> a
roundUp m n = n + mod (-n) m

indentUnits :: IndentParser st Int
indentUnits = do
  indentChars <- many (oneOf " \t")
  return (foldl' addUnits 0 indentChars)
  where addUnits n ' '  = n + 1
        addUnits n '\t' = roundUp 8 (n + 1)
