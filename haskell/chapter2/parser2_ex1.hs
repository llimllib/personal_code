module Main where
import System.Environment
import IO
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad

{- Exercise 1.1: Rewrite parseNumber using do-notation.
 - From the prelude:
 - 
The monadic lifting operators promote a function to a monad. The function arguments are scanned left to right. For example,

liftM2 (+) [0,1] [0,2] = [0,2,1,3]
liftM2 (+) (Just 1) Nothing = Nothing

...snip..

liftM            :: (Monad m) => (a -> b) -> (m a -> m b)
liftM f          =  \a -> do { a' <- a; return (f a') }

> :t (many1 digit)
(many1 digit) :: GenParser Char st [Char]

> :t (Number . read) $ (many1 digit)
    Couldn't match expected type `String'
           against inferred type `GenParser Char st [Char]'
            -- illegal because many1 digit doesn't return a string

> :t liftM (Number . read) $ (many1 digit)
liftM (Number . read) $ (many1 digit) :: GenParser Char st LispVal

> :t (Number . read)
(Number . read) :: String -> LispVal

So, it seems that (many1 digit) returns some sort of parser string, where (Number . read)
expects a real String. liftM, then, "promotes" (Number . read) into a function which takes
a value of type a and calls "return" on it, lifting it into the appropriate monad. So, how
about:
-}
-- parseNumber = do return (Number . read) $ (many1 digit)

{- This doesn't work; the parseNumber function compiles, but has the wrong type. From the
 - interpreter:
 -
    Couldn't match expected type `GenParser Char st LispVal'
           against inferred type `String -> LispVal'
    In the second argument of `(<|>)', namely `parseNumber'
    In the second argument of `(<|>)', namely
        `parseString <|> parseNumber'
    In the expression: parseAtom <|> (parseString <|> parseNumber)

    So, we're expected to return a GenParser LispVal, but instead return a string. Maybe
    I just need to return the *result* of the function:
-}
-- parseNumber = do return $ (Number . read) $ (many1 digit)

{- This is also an error:
 -
    Couldn't match expected type `String'
           against inferred type `GenParser Char st [Char]'
      Expected type: String
      Inferred type: GenParser Char st [Char]

because I forgot that (Number . read) $ (many1 digit) already doesn't work; I need to get
(Number . read) into the Parser monad so that it expects and can deal with something other
than a String.

liftm takes a function "f" from (a -> b), in this case (Number . read) which is String -> LispVal,
and returns a function of one argument (to which we give (many1 digit)) which does the following:
    assign the argument to a variable "a'" (!!? <- does something I don't understand.)
    return (f a')

From the manual (http://www.haskell.org/onlinereport/exps.html):
    A do expression provides a more conventional syntax for monadic programming. It allows an 
    expression such as

      putStr "x: "    >> 
      getLine         >>= \l ->
      return (words l)

    to be written in a more traditional way as:

      do putStr "x: "
         l <- getLine
         return (words l)
So it shouldn't change the type at all. WTF! TODO: email haskell-cafe.
-}
parseNumber = do
    digits <- (many1 digit)
    return ((Number . read) digits)

-- is equivalent to

parseNumberNoDo = many1 digit >>= \l -> return ((Number . read) l)

-- which is not equivalent to

-- parseNumberWrong = return ((Number . read) (many1 digit))
-- because it's a sequence thing? If so, why does liftM work?
--   because we pass (Number . read) to it, not the whole series;
--   it translates (Number . read) into a function which does exactly
--   what we see above.


{------------------------------------------
 - What follows is the code from parser2.hs, mostly unchanged
 - -----------------------------------------}


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol = oneOf "!$#%&|*+-/:<=>?@^_~"
spaces = skipMany1 space
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 {-"String" here means the constructor from the LispVal struct
                 "$" means right-associate, instead of left. Otherwise, we
                 could have written "return (String x)"
                  Q: do we need it here, or did the author mean that we'd use
                     it in the future?
                  A: we need it here, so it's not parsed as (return String) x
                     (right?)
                 -}
                 return $ String x

parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               --convert first into a list so we can use the list concat
               --operator (strings = lists in haskell)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          {-otherwise is not a keyword; just here for
                            readability. Actually, a variable called 
                            "otherwise" gets bound, we just ignore it
                          -}
                          otherwise -> Atom atom

parseExpr = parseAtom <|> parseString <|> parseNumber

--args to parse function are: parser name-of-parser text-to-parse
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main = do args <- getArgs
          putStrLn (args !! 0)
          putStrLn (readExpr (args !! 0))
