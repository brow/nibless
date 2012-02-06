module Xocolatl.Combinators
  ( ignoring
  , manyTill1
  ) where

import Text.ParserCombinators.Parsec
  
ignoring :: GenParser tok st a -> GenParser tok st b -> GenParser tok st a
ignoring p ignore = do
  skipMany ignore
  result <- p
  skipMany ignore
  return result

manyTill1 :: GenParser tok st a -> GenParser tok st end -> GenParser tok st [a]
manyTill1 p end = do
  x <- p
  xs <- manyTill p end
  return (x:xs)
