import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

spacesParser :: Parser()
spacesParser = skipMany1 space

symbolParser :: Parser Char
symbolParser = oneOf "!#$%&|*+-/:<=>?@^_~"

{-
TODO #2 about >> (bind) operator
It was used behind the scenes to combine the lines of a do-block. Here, we use it explicitly to combine our whitespace and symbol parsers. However, bind has completely different semantics in the Parser and IO monads. In the Parser monad, bind means "Attempt to match the first parser, then attempt to match the second with the remaining input, and fail if either fails." In general, bind will have wildly different effects in different monads; it's intended as a general way to structure computations, and so needs to be general enough to accommodate all the different types of computations.
-}

readExpr :: String -> String
readExpr input = case parse (spacesParser >> symbolParser) "lisp" input of -- TODO #2: What does >> actually signify?
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO()
main = do
    (expr:_) <- getArgs -- TODO #1: what does (expr:_) means?
    putStrLn (readExpr expr)