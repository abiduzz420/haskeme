module Haskeme.Parser where

import           Control.Monad
import           Control.Monad.Except
import           Prelude
import           Text.ParserCombinators.Parsec

import           Haskeme.Core

-- # Parsing

-- parseString is a parser action
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x -- String constructor from LispVal. return injects LispVal into Parser monad

-- We're back to using the do-notation instead of the >> operator. This is because we'll be retrieving the value of our parse (returned by many(noneOf "\"")) and manipulating it, 
-- interleaving some other parse operations in the meantime. In general, use >> if the actions don't return a value, >>= if you'll be immediately passing
-- that value into the next action, and do-notation
-- otherwise.

-- Each line of a do-block must have the same type, but the result of our String constructor is just a plain old LispVal. 
-- return lets us wrap that up in a Parser action that consumes no input
-- but returns it as the inner value. Thus, the whole parseString action will have type Parser LispVal

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> symbol <|> digit)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

-- Here, we introduce another Parsec combinator, the choice operator <|>. This tries the first parser, then if it fails, tries the second.
-- If either succeeds, then it returns the value returned by that parser.
-- The first parser must fail before it consumes any input

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r

-- the result of many1 digit is actually a Parser String(m a1), so our combined (Number . read) still can't operate on it.
-- We need a way to tell it to just operate on the value(a1) inside the monad(m), giving us back a Parser LispVal(m r).
-- The standard function liftM does exactly that, so we apply liftM to our (Number . read) function, and then apply the result of that to our parser.

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces'

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

-- identifies '(foo bar)
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- Different from Text.ParserCombinators.Parsec.spaces
spaces' :: Parser ()
spaces' = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- the haskeme parser
-- TODO: cannot parse "(define x 2   )"
parseExpr :: Parser LispVal
parseExpr = 
      parseAtom 
  <|> parseString 
  <|> parseNumber 
  <|> parseQuoted 
  <|> do
    spaces >> char '(' >> spaces
    x <- try parseList <|> parseDottedList
    spaces >> char ')' >> spaces
    return x

readOrThrows :: Parser a -> String -> ThrowsError a
readOrThrows parser input = case parse parser "lisp" input of
  Left  err -> throwError $ Parser err
  Right val -> return val

-- reading the input string from repl
readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrows parseExpr
-- reading input from a file
readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrows (endBy parseExpr spaces')
-- ^ (endBy parseExpr spaces') returns Parser [LispVal]
