module Haskeme where

import           Control.Monad
import           Prelude
import           System.IO                      ( hFlush
                                                , hPutStrLn
                                                , stderr
                                                , stdout
                                                )

import           Haskeme.Core
import           Haskeme.Eval                   ( eval
                                                , evalAndPrint
                                                )
import           Haskeme.Primitives             ( primitiveBindings )
import           Haskeme.Vars                   ( bindVars )

-- | The REPL

runRepl :: IO ()
runRepl =
  primitiveBindings >>= until_ (== "quit") (readPrompt "λ> ") . evalAndPrint

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings
    >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  runIOThrows (liftM show $ eval env (List [Atom "load", String $ head args]))
    >>= hPutStrLn stderr
-- ^ args !! 0 is the filename (first argument)
-- ? I am not sure why rest of the argument list is taken into "args"

-- Helper Functions

-- result variable captures the input
until_ :: Monad m => (t -> Bool) -> m t -> (t -> m a) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result then return () else action result >> until_ pred prompt action

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

