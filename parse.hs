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

ignoring :: Stream s m t => ParsecT s u m a -> ParsecT s u m () -> ParsecT s u m [a]
ignoring p ignore = many $ do
  ignore
  result <- p
  ignore
  return result

markup :: IndentCharParser () [View]
markup = do
  views <- view `ignoring` spacesAndComments
  eof
  return views

spacesAndComments :: IndentCharParser () ()
spacesAndComments = skipMany (void space <|> void comment)

comment :: IndentCharParser () String
comment = do
  string "/*"
  manyTill anyChar (try (string "*/"))

cIdentifier :: IndentCharParser () String
cIdentifier = many1 (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ "_"))

view :: IndentCharParser () View
view = do
  char '#'
  cIdentifier
  return (View "UIView" Nothing Map.empty [])

main = parseFromFile markup "markup2.txt" >>= print
