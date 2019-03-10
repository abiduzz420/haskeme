module Main where
import System.Environment

main :: IO()
main = do
    name <- getLine
    putStrLn $ "My name is " ++ name