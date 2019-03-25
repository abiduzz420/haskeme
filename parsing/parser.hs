import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal]
               LispVal
  | Number Integer
  | String String
  | Bool Bool

-- parseString is a parser action
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x -- String constructor from LispVal. return injects LispVal into Parser monad

{-
    We're back to using the do-notation instead of the >> operator. This is because we'll be retrieving the value of our parse (returned by many(noneOf "\"")) and manipulating it, interleaving some other parse operations in the meantime. In general, use >> if the actions don't return a value, >>= if you'll be immediately passing that value into the next action, and do-notation otherwise.
    
    Each line of a do-block must have the same type, but the result of our String constructor is just a plain old LispVal. return lets us wrap that up in a Parser action that consumes no input but returns it as the inner value. Thus, the whole parseString action will have type Parser LispVal
-}
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> symbol <|> digit)
  let atom = first : rest
  return $
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _ -> Atom atom

{-
    Here, we introduce another Parsec combinator, the choice operator <|>. This tries the first parser, then if it fails, tries the second. If either succeeds, then it returns the value returned by that parser. The first parser must fail before it consumes any input
-}
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

{-
	 the result of many1 digit is actually a Parser String(i.e m a1), so our combined (Number . read) still can't operate on it. We need a way to tell it to just operate on the value(a1) inside the monad(m), giving us back a Parser LispVal(m r). The standard function liftM does exactly that, so we apply liftM to our (Number . read) function, and then apply the result of that to our parser.
	 
   liftM :: Monad m => (a1 -> r) -> m a1 -> m r
-}
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumber <|> parseQuoted <|> do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value " ++ show val

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (Number number) = show number
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList init last) = "(" ++ unwordsList init ++ " . " ++ show last ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

{-
		TODO #2 answer about >> (bind) operator
			`case parse (spaces >> symbol) "lisp" input of`
    It was used before behind the scenes to combine the lines of a do-block. Here, we use it explicitly to combine our whitespace and symbol parsers. However, bind has completely different semantics in the Parser and IO monads. In the Parser monad, bind means "Attempt to match the first parser, then attempt to match the second with the remaining input, and fail if either fails." In general, bind will have wildly different effects in different monads; it's intended as a general way to structure computations, and so needs to be general enough to accommodate all the different types of computations.
-}

main :: IO ()
main = do
  (expr:_) <- getArgs -- TODO #1: what does (expr:_) means?
  putStrLn (readExpr expr)

-- home work exercises 2.1
-- parseNumber with >>=
parseNumber' :: Parser LispVal
parseNumber' = many1 digit >>= return . Number . read

-- parseNumber with do-notation
parseNumber'' :: Parser LispVal
parseNumber'' = do
  parseStr <- many1 digit
  return . Number . read $ parseStr
