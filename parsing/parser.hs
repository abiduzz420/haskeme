import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbolParser :: Parser Char
symbolParser = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse symbolParser "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO()
main = do
    args <- getLine
    putStrLn $ (readExpr args)