module Haskeme.Core where

import           Control.Monad.Except
import           Data.IORef
import           System.IO
import           Text.ParserCombinators.Parsec

data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal]
                 LispVal
    | Number Integer
    | String String
    | Bool Bool
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    | Func { params :: [String], vaarg :: Maybe String,
             body :: [LispVal], closure :: Env }
    | Port Handle
    | IOFunc ([LispVal] -> IOThrowsError LispVal)

instance Show LispVal where
    show = showVal

-- LispVal printer
showVal :: LispVal -> String
showVal (Atom   name    ) = name
showVal (Number number  ) = show number
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Bool   True    ) = "#t"
showVal (Bool   False   ) = "#f"
showVal (List   contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList init last) =
    "(" ++ unwordsList init ++ " . " ++ show last ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc        _) = "<IO primitive>"
showVal (Port          _) = "<IO Port>"
showVal Func { params = args, vaarg = vaargs, body = body, closure = env } =
    "(lambda ("
        ++ unwords (map show args)
        ++ (case vaargs of
               Nothing  -> ""
               Just arg -> " . " ++ arg
           )
        ++ ") ...)"

-- make a string out of LispVal list with spaces
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- IORef enables the use of stateful variables inside the IO Monad
type Env = IORef [(String, IORef LispVal)]

-- creates an empty environment with []. `IORef`s can only be used inside IO Monad hence IO Env is the type
nullEnv :: IO Env
nullEnv = newIORef []

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String LispVal
               | UnboundVar String String
               | Default String

instance Show LispError where
    show = showError

showError :: LispError -> String
showError (NumArgs expected found) =
    "Expected " ++ show expected ++ " args, Found values " ++ unwordsList found
showError (TypeMismatch expected found) =
    "Expected " ++ show expected ++ " args, found values " ++ show found
showError (Parser parseErr               ) = "Parse Error at " ++ show parseErr
showError (BadSpecialForm message form   ) = message ++ ": " ++ show form
showError (NotFunction    message func   ) = message ++ ": " ++ show func
showError (UnboundVar     message varname) = message ++ ": " ++ varname

type ThrowsError = Either LispError

-- takes error values and convert them into their string representation
trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- Dealing with two monads: Error and IO. IOThrowsError is a combined monad: A monad which may contain IO actions that throw a LispError
-- Monad transformer allows combining two monads, ExceptT is one such monad transformer
-- It lets us layer error-handling functionality on top of IO monad
type IOThrowsError = ExceptT LispError IO

-- Methods in typeclasses resolve based on the type of the expression, 
-- so throwError and return (members of MonadError and Monad, respectively) take on their 
-- IOThrowsError definitions
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left  err) = throwError err
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
