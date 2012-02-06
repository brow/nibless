import Text.ParserCombinators.Parsec
import Control.Monad
import Xocolatl.Combinators

beginComment :: GenParser Char () String
beginComment = string "/*"

endComment :: GenParser Char () String
endComment = string "*/"

commented :: GenParser Char () String
commented = do
  begin <- beginComment 
  body <- manyTill anyChar (lookAhead endComment)
  end <- endComment
  return (begin ++ body ++ end)

uncommented :: GenParser Char () String
uncommented = manyTill1 anyChar (eof <|> void (lookAhead beginComment))

blankComments :: GenParser Char () String
blankComments = many (blankCommented <|> uncommented) >>= return . concat
  where blankCommented = fmap (map blank) commented
        blank '\n' = '\n'
        blank _ = ' '
