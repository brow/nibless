module Xocolatl.Combinators where

import Text.Parsec
import Text.Parsec.Char
  
ignoring :: Stream s m t => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m a
ignoring p ignore = do
  ignore
  result <- p
  ignore
  return result


