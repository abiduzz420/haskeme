module Haskeme.Primitives where

import           Control.Monad
import           Control.Monad.Except
import           System.IO

import           Haskeme.Core
import           Haskeme.Parser
import           Haskeme.Vars
import           Haskeme.Eval

-- # Primitive functions

-- k-v to map scheme functions to haskell arithmetic operations
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+"        , numericBinop (+))
  , ("-"        , numericBinop (-))
  , ("*"        , numericBinop (*))
  , ("/"        , numericBinop div)
  , ("mod"      , numericBinop mod)
  , ("remainder", numericBinop rem)
  , ("quotient" , numericBinop quot)
  , ("&&"       , boolBoolBinop (&&))
  , ("||"       , boolBoolBinop (||))
  , ("="        , numBoolBinop (==))
  , ("/="       , numBoolBinop (/=))
  , (">"        , numBoolBinop (>))
  , ("<"        , numBoolBinop (<))
  , (">="       , numBoolBinop (>=))
  , ("<="       , numBoolBinop (<=))
  , ("string=?" , strBoolBinop (==))
  , ("string>?" , strBoolBinop (>))
  , ("string<?" , strBoolBinop (<))
  , ("string>=?", strBoolBinop (>=))
  , ("string<=?", strBoolBinop (<=))
  , ("car"      , car)
  , ("cdr"      , cdr)
  , ("cons"     , cons)
  , ("eqv?"     , eqv)
  , ("eq?"      , eqv)
  , ("equal?"   , equal)
  ]

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv
    >>= (  flip bindVars
        $  (map makePrimitiveFunc primitives)
        ++ (map makeIOFunc ioPrimitives)
        )
 where
  makeFunc constructor (var, func) = (var, constructor func)
  makePrimitiveFunc
    :: (String, [LispVal] -> ThrowsError LispVal) -> (String, LispVal)
  makePrimitiveFunc = makeFunc PrimitiveFunc
  makeIOFunc
    :: (String, [LispVal] -> IOThrowsError LispVal) -> (String, LispVal)
  makeIOFunc = makeFunc IOFunc

boolBinop
  :: (LispVal -> ThrowsError a)
  -> (a -> a -> Bool)
  -> [LispVal]
  -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
  then throwError $ NumArgs 2 args
  else do
    left  <- unpacker $ head args
    right <- unpacker $ args !! 1
    return $ Bool $ left `op` right

boolBoolBinop = boolBinop unpackBool
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String str ) = return str
unpackStr (Bool   bool) = return $ show bool
unpackStr (Number n   ) = return $ show n
unpackStr notString     = throwError $ TypeMismatch "string" notString

-- numeric binary operations function
numericBinop
  :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op single_val@[_] = throwError $ NumArgs 2 single_val
numericBinop op args = mapM unpackNum args >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)]
  in  if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do
      unpackerArg1 <- unpacker arg1
      unpackerArg2 <- unpacker arg2
      return $ unpackerArg1 == unpackerArg2
    `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM
    (unpackEquals arg1 arg2)
    [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ primitiveEquals || let (Bool x) = eqvEquals in x
equal badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool   arg1, Bool arg2  ] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom   arg1, Atom arg2  ] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] =
  eqv [List (xs ++ [x]), List (ys ++ [y])]
eqv [List arg1, List arg2] = return $ Bool $ length arg1 == length arg2 && all
  eqvPair
  (zip arg1 arg2)
 where
  eqvPair (x, y) = case eqv [x, y] of
    Left  err        -> False
    Right (Bool val) -> val
eqv [_, _]     = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)        ] = return x
car [DottedList (x : xs) _] = return x
car [badArg               ] = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)        ] = return $ List xs
cdr [DottedList [_     ] x] = return x
cdr [DottedList (_ : xs) y] = return $ DottedList xs y
cdr [badArg               ] = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x , List []        ] = return $ List [x]
cons [x , List xs        ] = return $ List $ x : xs
cons [x , DottedList xs y] = return $ DottedList (x : xs) y
cons [x1, x2             ] = return $ DottedList [x1] x2
cons badArgList            = throwError $ NumArgs 2 badArgList

-- io primitives reacting to outside world

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
  [ ("apply"            , applyProc)
  , ("open-input-file"  , makePort ReadMode)
  , ("open-output-file" , makePort WriteMode)
  , ("close-input-port" , closePort)
  , ("close-output-port", closePort)
  , ("read"             , readProc)
  , ("write"            , writeProc)
  , ("read-contents"    , readContents)
  , ("read-all"         , readAll)
  ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> ([LispVal] -> IOThrowsError LispVal)
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO (hClose port) >> return (Bool True)

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename


