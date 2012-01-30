import Text.ParserCombinators.Parsec.IndentParser 
import Data.Map as Map
import Text.Parsec
import Data.Char (isSeparator)
import Control.Monad

type Property = String
type Value = String
type Style = Map.Map Property Value
data View = View  { klass :: String
                  , outlet :: Maybe String
                  , style :: Style
                  , subviews :: [View]
                  } deriving Show                

ignoring :: Stream s m t => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m a
ignoring p ignore = do
  ignore
  result <- p
  ignore
  return result

markup :: IndentCharParser () [View]
markup = do
  views <- many (view `ignoring` spacesAndComments)
  eof
  return views

spacesAndComments :: IndentCharParser () ()
spacesAndComments = skipMany (void space <|> void comment)

comment :: IndentCharParser () String
comment = do
  string "/*"
  manyTill anyChar (try (string "*/"))

cIdentifier :: IndentCharParser () String
cIdentifier = do
  head <- cInitial
  tail <- many cNonInitial
  return (head : tail)
  where cInitial = letter <|> char '_'
        cNonInitial = cInitial <|> digit

view :: IndentCharParser () View
view = do
  char '#'
  cIdentifier <?> "identifier"
  return (View "UIView" Nothing Map.empty [])

main = parseFromFile markup "markup2.txt" >>= print
