import Nibless.Core
import Nibless.Combinators
import Data.Map as Map
import Text.ParserCombinators.Parsec.IndentParser 
import Text.Parsec
import Control.Monad
import Text.Show.Pretty (ppShow)
import System.Environment (getArgs)

markup :: IndentCharParser () [View]
markup = do
  views <- many (viewElem `ignoring` spacesAndComments)
  eof
  return views

spacesAndComments :: IndentCharParser () ()
spacesAndComments = skipMany (void space <|> void comment)

comment :: IndentCharParser () String
comment = do
  string "/*"
  manyTill anyChar (try (string "*/"))

cIdentifier :: IndentCharParser () Identifier
cIdentifier = do
  head <- cInitial
  tail <- many cNonInitial
  return (head : tail)
  where cInitial = letter <|> char '_'
        cNonInitial = cInitial <|> digit

viewElem :: IndentCharParser () View
viewElem = do
  subclass <- optionMaybe subclassAttr
  outlet <- optionMaybe outletAttr
  if (subclass, outlet) == (Nothing, Nothing)
  then
    fail "either class or outlet is required"
  else do
    style <- optionMaybe styleAttr 
    return (View subclass outlet style [])

outletAttr :: IndentCharParser () Identifier
outletAttr = do
  char '#'
  cIdentifier <?> "identifier"

subclassAttr :: IndentCharParser () Identifier
subclassAttr = do
  cIdentifier <?> "identifier"

styleAttr :: IndentCharParser () Style
styleAttr = do
  assocList <- braces (sepBy pair (char ',')) 
  return (Map.fromList assocList)
  where pair = stylePair `ignoring` spacesAndComments
      
stylePair :: IndentCharParser () (Identifier, Value)
stylePair = do
  identifier <- cIdentifier
  spacesAndComments
  char ':'
  spacesAndComments 
  val <- value
  spacesAndComments
  return (identifier, val)

stringValue :: IndentCharParser () Value
stringValue = do
  s <- quotes $ many $ quotedChar
  return (NSString s)
  where quotedChar = noneOf "\"" <|> try (string "\\\"" >> return '"')  

expressionValue :: IndentCharParser () Value
expressionValue = do
 cIdentifier >>= return . Expression 
value :: IndentCharParser () Value 
value = (stringValue      <?> "quoted string")    <|>
        (expressionValue  <?> "Obj-C expression") 

braces = between (char '{') (char '}')

quotes = between (char '"') (char '"')

main = do 
  args <- getArgs
  parseFromFile markup (args !! 0) >>= putStrLn . ppShow
