module Haskeme.Vars where

import Control.Monad
import Control.Monad.Except
import Data.IORef
import Prelude

import Haskeme.Core

-- # support for variables

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
