module PythonIndent.Prim
  ( IndentParser
  , getState
  , setState
  , runParser
  , parse
  , parseFromFile
  , parseTest ) where

import Control.Monad
import qualified Text.ParserCombinators.Parsec.Prim as Prim
import Text.ParserCombinators.Parsec hiding (getState, setState, runParser, 
                                              parse, parseFromFile, parseTest)

type IndentParser st a = GenParser Char (st, [Int]) a

getState  :: IndentParser st st
getState = fmap fst Prim.getState

setState :: st -> IndentParser st ()
setState st = do 
  indentStack <- fmap snd Prim.getState 
  Prim.setState (st, indentStack)


pushIndentLevel :: Int -> IndentParser st ()
pushIndentLevel n = do
  (st, indentStack) <- Prim.getState
  Prim.setState (st, n:indentStack)

popIndentLevel :: IndentParser st ()
popIndentLevel = do
  (st, indentStack) <- Prim.getState
  case indentStack of
    [0] -> fail "popped last indent level"
    xs -> Prim.setState (st, tail xs)

indentLevel :: IndentParser st Int
indentLevel = Prim.getState >>= (return . head . snd)


indentUnits :: IndentParser st Int
indentUnits = try (char ' ' >> fmap (+1) indentUnits) <|> return 0  

{-| 
  
The most generic way to run an IndentParser. Use @parseTest@ for
testing your parser instead.

-}

runParser :: IndentParser st a  -- ^ the parser to be run
             -> st  -- ^ the initial state 
             -> SourceName -- ^ the source file name
             -> String  -- ^ the list of tokens
             -> Either ParseError a -- ^ the result of parsing
runParser p st sname = Prim.runParser p (st, []) sname

{-|

Runs the given parser on a given input stream and returns either the
result or parse error.

-}

parse :: IndentParser () a -- ^ The parser to run
         -> SourceName  -- ^ The name of the source (to report errors)
         -> String   -- ^ The input to the parser 
         -> Either ParseError a
parse p = runParser p ()

{-| 

Like @'parse'@ but use the contents of @SourceName@ as the input
tokens.

-}

parseFromFile :: IndentParser () a -- ^ The parser to run
              -> SourceName  -- ^ The file on which to run.
              -> IO (Either ParseError a) 
parseFromFile p fpath = do str <- readFile fpath
                           return $ parse p fpath str

{-| 

Runs the input parser on the given stream and prints the result.
Useful for testing parsers.

-}

parseTest :: Show a => 
             IndentParser () a -- ^ The parser to test
                 -> String -- ^ The input to the parser
                 -> IO ()
parseTest p input = case result of
                      Left err  -> do putStr "Error"; print err
                      Right a   -> do print a
    where result = runParser p () "" input


