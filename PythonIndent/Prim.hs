module PythonIndent.Prim
  ( IndentParser
  , getState
  , setState
  , getIndentStack
  , setIndentStack
  , runParser
  , parse
  , parseFromFile
  , parseTest ) where

import Control.Monad
import qualified Text.ParserCombinators.Parsec as P

type IndentParser st a = P.GenParser Char (st, [Int]) a

getState  :: IndentParser st st
getState = fmap fst P.getState

setState :: st -> IndentParser st ()
setState st = do 
  indentStack <- getIndentStack 
  P.setState (st, indentStack)

getIndentStack :: IndentParser st [Int]
getIndentStack = fmap snd P.getState

setIndentStack :: [Int] -> IndentParser st ()
setIndentStack indentStack = do
  st <- getState
  P.setState (st, indentStack)

runParser :: IndentParser st a  -- ^ the parser to be run
             -> st  -- ^ the initial state 
             -> P.SourceName -- ^ the source file name
             -> String  -- ^ the list of tokens
             -> Either P.ParseError a -- ^ the result of parsing
runParser p st sname = P.runParser p (st, [0]) sname

parse :: IndentParser () a -- ^ The parser to run
         -> P.SourceName  -- ^ The name of the source (to report errors)
         -> String   -- ^ The input to the parser 
         -> Either P.ParseError a
parse p = runParser p ()

parseFromFile :: IndentParser () a -- ^ The parser to run
              -> P.SourceName  -- ^ The file on which to run.
              -> IO (Either P.ParseError a) 
parseFromFile p fpath = do str <- readFile fpath
                           return $ parse p fpath str

parseTest :: Show a => 
             IndentParser () a -- ^ The parser to test
                 -> String -- ^ The input to the parser
                 -> IO ()
parseTest p input = case result of
                      Left err  -> do putStr "Error"; print err
                      Right a   -> do print a
    where result = runParser p () "" input


