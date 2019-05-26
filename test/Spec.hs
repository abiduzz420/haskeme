import           Test.Tasty
import           Test.Tasty.HUnit

import           Haskeme.Core
import           Haskeme.Eval
import           Haskeme.Parser
import           Haskeme.Primitives

main :: IO()
main = defaultMain tests

-- TODO: create (Eq LispVal) so as to not deal with Strings
printVal :: ThrowsError LispVal -> String
printVal = show . extractValue

tests :: TestTree
tests = testGroup "\nhaskeme all-tests" [unitTests, qcTests]

unitTests :: TestTree
unitTests = testGroup "unit tests" [prims]

qcTests :: TestTree
qcTests = testGroup "quickcheck" []

-- § Primitives

prims :: TestTree
prims = testGroup "primitives" [carT, cdrT, consT, arithmeticT, boolT, stringT, eqvT]

carT :: TestTree
carT = testCase "first element from list" $ do
    printVal (car [List [Number 1, Number 2]]) @?= "1"
    printVal (car [DottedList [Atom "a", Atom "b"] (Atom "c")]) @?= "a"

cdrT :: TestTree
cdrT = testCase "second element from pair" $ do
    printVal (cdr [List [Number 1, Number 2, Number 3]]) @?= "(2 3)"
    printVal (cdr [DottedList [Atom "a", Atom "b"] (Atom "c")]) @?= "(b . c)"
    printVal (cdr [DottedList [Atom "a"] (Atom "c")]) @?= "c"

consT :: TestTree
consT = testCase "add me into the mix" $ do
    printVal (cons [Number 1, List []]) @?= "(1)"
    printVal (cons [Number 1, List [Number 2, Number 3]]) @?= "(1 2 3)"
    printVal (cons [Atom "a", DottedList [Atom "b", Atom "c"] (Atom "d")])
        @?= "(a b c . d)"
    printVal (cons [Atom "a", Atom "b"]) @?= "(a . b)"

arithmeticT :: TestTree
arithmeticT = testCase "mind your arithmetic" $ do
    printVal (numericBinop (+) [Number 1, Number 2, Number 3]) @?= "6"
    printVal (numericBinop (*) [Number 2, Number 2, Number 3]) @?= "12"
    printVal (numericBinop (-) [Number 29, Number 2, Number 3]) @?= "24"
    printVal (numericBinop div [Number 20, Number 2]) @?= "10" -- Does not support floating points (/ 20 3) ~ 6.666666666666667
    printVal (numericBinop mod [Number 20, Number 3]) @?= "2"

boolT :: TestTree
boolT = testCase "life aint black and white" $ do
    printVal (boolBoolBinop (&&) [Bool True, Bool True]) @?= "#t"
    printVal (boolBoolBinop (&&) [Bool True, Bool False]) @?= "#f"
    printVal (boolBoolBinop (&&) [Bool False, Bool False]) @?= "#f"
    printVal (boolBoolBinop (||) [Bool True, Bool True]) @?= "#t"
    printVal (boolBoolBinop (||) [Bool False, Bool True]) @?= "#t"
    printVal (boolBoolBinop (||) [Bool False, Bool False]) @?= "#f"
    printVal (numBoolBinop (==) [Number 1, Number 2]) @?= "#f"
    printVal (numBoolBinop (==) [Number 2, Number 2]) @?= "#t"
    printVal (numBoolBinop (/=) [Number 1, Number 2]) @?= "#t"
    printVal (numBoolBinop (/=) [Number 1, Number 1]) @?= "#f"
    printVal (numBoolBinop (>) [Number 1, Number 2]) @?= "#f"
    printVal (numBoolBinop (<) [Number 1, Number 2]) @?= "#t"
    printVal (numBoolBinop (>=) [Number 1, Number 1]) @?= "#t"
    printVal (numBoolBinop (>=) [Number 0, Number 2]) @?= "#f"
    printVal (numBoolBinop (<=) [Number 1, Number 2]) @?= "#t"
    printVal (numBoolBinop (<=) [Number 2, Number 2]) @?= "#t"

stringT :: TestTree
stringT = testCase "comparing strings" $ do
    printVal (strBoolBinop (==) [String "lisp", String "lisp"]) @?= "#t"
    printVal (strBoolBinop (==) [String "lisp", String "lisp"]) @?= "#t"
    printVal (strBoolBinop (<=) [String "hello", String "world"]) @?= "#t"

-- TODO: equal? primitive tests not added because it does not meet the scheme specifications
-- (equal? 3 "3") => (expected: #f) but haskeme outputs #t due to usage of unpacker

eqvT :: TestTree
eqvT = testCase "are they equivalent?" $ do
    printVal (eqv [Number 1, Number 3]) @?= "#f"
    printVal (eqv [Number 1, Number 1]) @?= "#t"
    printVal (eqv [Atom "hello", Atom "hello"]) @?= "#t"
    printVal (eqv [Atom "hello", Atom "Hello"]) @?= "#f"
    printVal (eqv [String "hello", String "hello"]) @?= "#t"
    printVal (eqv [String "hello", String "Hello"]) @?= "#f"
    printVal (eqv [Bool True, Bool True]) @?= "#t"
    printVal (eqv [Bool True, Bool False]) @?= "#f"
    printVal (eqv [Bool False, Bool False]) @?= "#t"
    printVal (eqv [DottedList [Atom "a", Atom "b"] (Atom "c"),
                   DottedList [Atom "a", Atom "b"] (Atom "c")]
                )  @?= "#t"
    printVal (eqv [DottedList [Atom "b"] (Atom "c"),
                   DottedList [Atom "b"] (Atom "c")]
                )  @?= "#t"
    printVal (eqv [DottedList [Atom "a", Atom "b", Atom "d"] (Atom "c"),
                   DottedList [Atom "a", Atom "b"] (Atom "c")])
                   @?= "#f"
    printVal (eqv [List [Number 1, Number 2, Number 3],
                   List [Number 1, Number 2, Number 3]])
                   @?= "#t"
    printVal (eqv [List [],
                   List [Number 1, Number 2, Number 3]])
                   @?= "#f"
    printVal (eqv [List [Number 1, Number 2],
                   List [Number 1, Number 2, Number 3]])
                   @?= "#f"
    printVal (eqv [List [Number 1, Number 2, Number 3],
                   List [Number 1, Number 2, Number 3]])
                   @?= "#t"
    printVal (eqv [List [Number 1, Atom "add", String "x", Bool True],
                   List [Number 1, Atom "add", String "x", Bool True]])
                   @?= "#t"
    printVal (eqv [List [Number 1, Number 2],
                   List [Number 1]])
                   @?= "#f"

-- § Variables
