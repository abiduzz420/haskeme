{-# LANGUAGE ExistentialQuantification #-}
import Control.Monad
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except
import Data.IORef

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal]
               LispVal
  | Number Integer
  | String String
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func { params :: [String], vaarg :: (Maybe String),
           body :: [LispVal], closure :: Env }
  | Port Handle
  | IOFunc ([LispVal] -> IOThrowsError LispVal)

instance Show LispVal where show = showVal

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
  rest <- many (letter <|> symbol <|> digit)
  let atom = first : rest
  return $
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _ -> Atom atom

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

-- the haskeme parser
parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumber <|> parseQuoted <|> do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x

readOrThrows :: Parser a -> String -> ThrowsError a
readOrThrows parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

-- reading the input string from repl
readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrows parseExpr
-- reading input from a file
readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrows (endBy parseExpr spaces)
-- ^ (endBy parseExpr spaces) returns Parser [LispVal]

-- The notation val@(String _) matches against any LispVal that's a string and then binds val to the whole LispVal,
-- and not just the contents of the String constructor. The result has type LispVal instead of type String.
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)
-- ^ we are using mapM instead of map is because eval returns a monad and not just value
eval env (List [Atom "if", pred, conseq, alt]) =
  do
    result <- eval env pred
    case result of
      Bool False -> eval env alt
      otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = -- "(define x 2)"
     eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = -- "(define (addThese x y) (+ x y))"
     makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) vaargs : body)) =
     makeVarArgs vaargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params vaargs : body)) =
     makeVarArgs vaargs env params body
eval env (List (Atom "lambda" : vaargs@(Atom _) : body)) =
     makeVarArgs vaargs env [] body
eval env (List (func : args)) = do
     func <- eval env func
     argVals <- mapM (eval env) args
     apply func argVals
eval env badForm = throwError $ BadSpecialForm "(unrecognized special form): " badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (IOFunc func) args = func args
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params vaargs body closure) args = 
      if num params /= num args && vaargs == Nothing -- ! Try without num just length
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs vaargs >>= evalBody
      where
        remainingArgs = drop (length params) args
        num = toInteger . length
        bindVarArgs args env = case args of
          Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
          Nothing -> return env
        evalBody env = liftM last $ mapM (eval env) body
apply notFunc _ = throwError $ NotFunction "Not a function" notFunc

makeFunc vaargs env params body = return $ Func (map showVal params) vaargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

-- # Primitive functions

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

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ (map makePrimitiveFunc primitives) ++ (map makeIOFunc ioPrimitives))
  where
    makeFunc constructor (var, func) = (var, constructor func)
    makePrimitiveFunc :: (String, [LispVal] -> ThrowsError LispVal) -> (String, LispVal)
    makePrimitiveFunc = makeFunc PrimitiveFunc
    makeIOFunc :: (String, [LispVal] -> IOThrowsError LispVal) -> (String, LispVal)
    makeIOFunc = makeFunc IOFunc

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

-- io primitives reacting to outside world

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> ([LispVal] -> IOThrowsError LispVal)
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO (hClose port) >> (return $ Bool True)

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

-- read a full file of statements. Not to confuse with `load` which evaluates values as Scheme exprs
load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

-- LispVal printer
showVal :: LispVal -> String
showVal (Atom name) = name
showVal (Number number) = show number
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList init last) = "(" ++ unwordsList init ++ " . " ++ show last ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO Port>"
showVal (Func {params = args, vaarg = vaargs, body = body, closure = env}) = 
    "(lambda (" ++ unwords (map show args) ++
        (case vaargs of
          Nothing -> ""
          Just arg -> " . " ++ arg) ++ ") ...)"

-- make a string out of LispVal list with spaces
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- About >> (bind) operator
-- case parse (spaces >> symbol) "lisp" input of`
-- It was used before behind the scenes to combine the lines of a do-block. Here, we use it explicitly to combine our whitespace and symbol parsers.
-- However, bind has completely different semantics in the Parser and IO monads. In the Parser monad, bind means "Attempt to match the first parser,
-- then attempt to match the second with the remaining input, and fail if either fails." In general, bind will have wildly different effects in different monads;
-- it's intended as a general way to structure computations, and so needs to be general enough to accommodate all the different types of computations.

-- # Error Checking and Exceptions

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String LispVal
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args, Found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Expected " ++ show expected
                                       ++ " args, found values " ++ show found
showError (Parser parseErr) = "Parse Error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname

instance Show LispError where show = showError

type ThrowsError = Either LispError

-- takes error values and convert them into their string representation
trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- # The REPL

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- # support for variables

-- IORef enables the use of stateful variables inside the IO Monad
type Env = IORef [(String, IORef LispVal)]

-- creates an empty environment with []. `IORef`s can only be used inside IO Monad hence IO Env is the type
nullEnv :: IO Env
nullEnv = newIORef []

-- Dealing with two monads: Error and IO. IOThrowsError is a combined monad: A monad which may contain IO actions that throw a LispError
-- Monad transformer allows combining two monads, ExceptT is one such monad transformer
-- It lets us layer error-handling functionality on top of IO monad
type IOThrowsError = ExceptT LispError IO

-- Methods in typeclasses resolve based on the type of the expression, 
-- so throwError and return (members of MonadError and Monad, respectively) take on their 
-- IOThrowsError definitions
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- runs IOThrowsError action and returns IO monad
-- this helper functions is used to interact with outside world and
-- at the same time separated from lazily evaluated pure functions
-- this method is quite similar to evalString method

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

-- ^ runExceptT :: IOThrowsError String -> IO (ThrowsError String)
-- IOThrowsError is ExceptT LispError IO
-- ThrowsError is Either LispError

-- checks if a variable is already defined or not
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

-- retrieve current value of the variable
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef -- returns a list (I guess?)
                       maybe (throwError $ UnboundVar "Getting an unbounded var" var)
                             (liftIO . readIORef)
                             (lookup var env) -- returns IOREf

-- ^ in throwError: We don't need to use liftIO to throw an error, however, because throwError is a defined for
-- the MonadError typeclass, of which ExceptT is an instance.

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Getting an unbounded var" var)
                                   (liftIO . (flip writeIORef value)) -- ? where is this value returned or how is it used
                                   (lookup var env)
                             return value

-- ^ writeIORef expects ref->value but ref is returned later, hence we use flip
-- which reverts the order, up to 3 arguments(?)
-- flip :: (a -> b -> c) -> c -> b -> a

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do 
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
       then setVar envRef var value >> return value
       else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef -- ? why no liftIO like earlier to retrieve the list
            writeIORef envRef ((var, valueRef) : env)
            return value
-- do-notation in else-block, creates an IO action, which is then lifted into IOThrowsError monad using liftIO
-- ? I'm still not entirely clear about how this is working: wondering whether `return value` returns IO monad or IOThrowsError monad

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

-- result captures the input
until_ :: Monad m => (t -> Bool) -> m t -> (t -> m a) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred resultliftM
     then return ()
     else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "λ> ") . evalAndPrint

runOne :: [String] -> IO ()
runOne args = do
     env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
     (runIOThrows $ liftM show $ eval env (List [Atom "load", String $ args !! 0]))
         >>= hPutStrLn stderr
-- ^ args !! 0 is the filename (first argument)
-- ? I am not sure why rest of the argument list is taken into "args"

main :: IO ()
main = do
     args <- getArgs
     if null args
      then runRepl
      else runOne $ args