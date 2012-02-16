module Pydent
  ( PydentParser
  , indentation
  , indented
  , manyIndented
  , emptyLine ) where

import Pydent.Prim
import Data.List (foldl')
import Control.Monad
import Text.ParserCombinators.Parsec

indentation :: PydentParser st ()
indentation = do
  consumed <- indentUnits
  indentLevel <- getIndentLevel
  case compare consumed indentLevel of
    EQ -> return ()
    GT -> unexpected "indent"
    LT -> unexpected "unindent"

indented :: PydentParser st a -> PydentParser st a
indented p = do
  indent
  x <- p
  dedent 
  return x

manyIndented :: PydentParser st a -> PydentParser st [a]
manyIndented p = do
  indent
  manyTill (skipEmptyLines >> indentation >> p) (try dedent)
  -- why is `try` needed above? `dedent` should never consume anything

indent :: PydentParser st ()
indent = do
  indentLevelAhead <- tryLookIndentUnits
  indentLevel <- getIndentLevel
  if indentLevelAhead > indentLevel
  then pushIndentLevel indentLevelAhead
  else expected "indent"

dedent :: PydentParser st ()
dedent = do
  indentLevelAhead <- tryLookIndentUnits
  indentLevel <- getIndentLevel
  if indentLevelAhead < indentLevel
  then void popIndentLevel
  else expected "unindent"

getIndentLevel :: PydentParser st Int
getIndentLevel = getIndentStack >>= return . head

pushIndentLevel :: Int -> PydentParser st ()
pushIndentLevel n = getIndentStack >>= setIndentStack . (n:)

popIndentLevel :: PydentParser st Int
popIndentLevel = do
  indentStack <- getIndentStack
  case indentStack of
    [0] -> fail "tried to pop base indent level"
    (x:xs) -> setIndentStack xs >> return x

roundUp :: Integral a => a -> a -> a
roundUp m n = n + mod (-n) m

indentUnits :: PydentParser st Int
indentUnits = fmap (foldl' addUnits 0) (many indentChar)
  where addUnits n '\t' = roundUp 8 (n + 1)
        addUnits n _  = n + 1

tryLookIndentUnits :: PydentParser st Int 
tryLookIndentUnits = (try . lookAhead) (skipEmptyLines >> indentUnits) 

expected :: String -> PydentParser st ()
expected = (void (oneOf []) <?>)

indentChar :: PydentParser st Char
indentChar = oneOf " \t"

emptyLine :: PydentParser st ()
emptyLine = many indentChar >> void newline

skipEmptyLines :: PydentParser s ()
skipEmptyLines = skipMany (try emptyLine) 
