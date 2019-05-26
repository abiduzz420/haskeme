module Main where

import Prelude
import System.Environment (getArgs)

import Haskeme (runRepl, runOne)

main :: IO ()
main = do
    args <- getArgs
    if null args
     then runRepl
     else runOne args
