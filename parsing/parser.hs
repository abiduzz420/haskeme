{-# LANGUAGE ExistentialQuantification #-}
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal]
               LispVal
  | Number Integer
  | String String
  | Bool Bool

instance Show LispVal where show = showVal

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
  liftM :: Monad m => (a1 -> r) -> m a1 -> m r
  
  the result of many1 digit is actually a Parser String(m a1), so our combined (Number . read) still can't operate on it. We need a way to tell it to just operate on the value(a1) inside the monad(m), giving us back a Parser LispVal(m r). The standard function liftM does exactly that, so we apply liftM to our (Number . read) function, and then apply the result of that to our parser.
-}

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

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

-- reading the input string
readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
{-
The notation val@(String _) matches against any LispVal that's a string and then binds val to the whole LispVal, and not just the contents of the String constructor. The result has type LispVal instead of type String.
-}

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
  do
    result <- eval pred
    case result of
      Bool False -> eval alt
      otherwise -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "(unrecognized special form): " badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args " func) ($ args) $ lookup func primitives

-- k-v to map scheme functions to haskell arithmetic operations
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
  ("+", numericBinop (+)),
  ("-", numericBinop (-)),
  ("*", numericBinop (*)),
  ("/", numericBinop div),
  ("mod", numericBinop mod),
  ("remainder", numericBinop rem),
  ("quotient", numericBinop quot),
  ("&&", boolBoolBinop (&&)),
  ("||", boolBoolBinop (||)),
  ("=", numBoolBinop (==)),
  ("/=", numBoolBinop (/=)),
  (">", numBoolBinop (>)),
  ("<", numBoolBinop (<)),
  (">=", numBoolBinop (>=)),
  ("<=", numBoolBinop (<=)),
  ("string=?", strBoolBinop (==)),
  ("string>?", strBoolBinop (>)),
  ("string<?", strBoolBinop (<)),
  ("string>=?", strBoolBinop (>=)),
  ("string<=?", strBoolBinop (<=)),
  ("car", car),
  ("cdr", cdr),
  ("cons", cons),
  ("eqv?", eqv),
  ("eq?", eqv),
  ("equal?", equal)]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do
                              left <- unpacker $ args !! 0
                              right <- unpacker $ args !! 1
                              return $ Bool $ left `op` right

boolBoolBinop = boolBinop unpackBool
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String str) = return str
unpackStr (Bool bool) = return $ show bool
unpackStr (Number n) = return $ show n
unpackStr notString = throwError $ TypeMismatch "string" notString

-- numeric binary operations function
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op single_val@[_] = throwError $ NumArgs 2 single_val
numericBinop op args = mapM unpackNum args >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0 
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return $ x
cdr [DottedList (_:xs) y] = return $ DottedList xs y
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, (List [])] = return $ List [x]
cons [x, (List xs)] = return $ List $ x:xs
cons [x, (DottedList xs y)] = return $ DottedList (x:xs) y
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List (xs ++ [x]), List (ys ++ [y])]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                 (all eqvPair (zip arg1 arg2))
    where
      eqvPair (x, y) = case eqv [x,y] of
                         Left err -> False
                         Right (Bool val) -> val                                            
eqv [_,_] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
  do unpackerArg1 <- unpacker arg1
     unpackerArg2 <- unpacker arg2
     return $ unpackerArg1 == unpackerArg2
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ primitiveEquals || let (Bool x) = eqvEquals in x
equal badArgList = throwError $ NumArgs 2 badArgList

-- LispVal printer
showVal :: LispVal -> String
showVal (Atom name) = name
showVal (Number number) = show number
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList init last) = "(" ++ unwordsList init ++ " . " ++ show last ++ ")"

-- make a string out of LispVal list with spaces
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

{-
		TODO #2 answer about >> (bind) operator
			`case parse (spaces >> symbol) "lisp" input of`
    It was used before behind the scenes to combine the lines of a do-block. Here, we use it explicitly to combine our whitespace and symbol parsers. However, bind has completely different semantics in the Parser and IO monads. In the Parser monad, bind means "Attempt to match the first parser, then attempt to match the second with the remaining input, and fail if either fails." In general, bind will have wildly different effects in different monads; it's intended as a general way to structure computations, and so needs to be general enough to accommodate all the different types of computations.
-}

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args, Found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Expected " ++ show expected
                                       ++ " args, found values " ++ show found
showError (Parser parseErr) = "Parse Error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ func
showError (UnboundVar message varname) = message ++ ": " ++ varname

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- TODO: (expr:_) <- getArgs .. what does (expr:_) means?
main :: IO ()
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (head args) >>= eval
     putStrLn $ extractValue $ trapError evaled
     -- >>= print . eval . readExpr . head -- TODO: Why is head used here?

-- home work exercises 2.1
-- parseNumber with >>=
parseNumber' :: Parser LispVal
parseNumber' = many1 digit >>= return . Number . read

-- parseNumber with do-notation
parseNumber'' :: Parser LispVal
parseNumber'' = do
  parseStr <- many1 digit
  return . Number . read $ parseStr