module Main where
import System.Environment

main :: IO()
main = do
    args <- getArgs
    putStrLn ("The sum is " ++ show (read (args !! 0) + read (args !! 1)))