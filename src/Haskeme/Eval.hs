module Haskeme.Eval where

import           Control.Monad
import           Control.Monad.Except
import           Prelude

import           Haskeme.Core
import           Haskeme.Parser
import           Haskeme.Vars

-- The notation val@(String _) matches against any LispVal that's a string and then binds val to the whole LispVal,
-- and not just the contents of the String constructor. The result has type LispVal instead of type String.
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _                  ) = return val
eval env val@(Number _                  ) = return val
eval env val@(Bool   _                  ) = return val
eval env (    Atom   id                 ) = getVar env id
eval env (    List   [Atom "quote", val]) = return val
eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)
-- ^ we are using mapM instead of map is because eval returns a monad values and not just normal values
-- Refer the function argument signature below:
-- ^ mapM :: (a -> m b) -> t a -> m (t b)
-- ^ map :: (a -> b) -> [a] -> [b]
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    otherwise  -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = -- "(define x 2)"
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = -- "(define (addThese x y) (+ x y))"
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) vaargs : body))
  = makeVarArgs vaargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params vaargs : body)) =
  makeVarArgs vaargs env params body
eval env (List (Atom "lambda" : vaargs@(Atom _) : body)) =
  makeVarArgs vaargs env [] body
eval env (List (func : args)) = do
  func    <- eval env func
  argVals <- mapM (eval env) args
  apply func argVals
eval env badForm =
  throwError $ BadSpecialForm "(unrecognized special form): " badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (IOFunc        func) args = func args
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params vaargs body closure) args =
  if num params /= num args && vaargs == Nothing -- ! Try without num just length
    then throwError $ NumArgs (num params) args
    else
      liftIO (bindVars closure $ zip params args)
      >>= bindVarArgs vaargs
      >>= evalBody
 where
  remainingArgs = drop (length params) args
  num           = toInteger . length
  bindVarArgs args env = case args of
    Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
    Nothing      -> return env
  evalBody env = liftM last $ mapM (eval env) body
apply notFunc _ = throwError $ NotFunction "Not a function" notFunc

makeFunc vaargs env params body =
  return $ Func (map showVal params) vaargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows $ liftM show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- read a full file of statements. Not to confuse with `load` which evaluates values as Scheme exprs
load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList
