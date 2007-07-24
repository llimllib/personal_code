module Main where
import System.Environment
import IO
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad

parseNumber = liftM (Number . read) $ (many1 digit)

{- 
 - Excercise 4: Change parseNumber to support the Scheme standard for different
 - bases. You may find the readOct and readHex functions useful.
 - 
 - From the scheme spec:

The syntax of the written representations for numbers is described formally in
section 7.1.1. Note that case is not significant in numerical constants.

A number may be written in binary, octal, decimal, or hexadecimal by the use of
a radix prefix. The radix prefixes are #b (binary), #o (octal), #d (decimal),
and #x (hexadecimal). With no radix prefix, a number is assumed to be expressed
in decimal.

A numerical constant may be specified to be either exact or inexact by a
prefix. The prefixes are #e for exact, and #i for inexact. An exactness prefix
may appear before or after any radix prefix that is used. If the written
representation of a number has no exactness prefix, the constant may be either
inexact or exact. It is inexact if it contains a decimal point, an exponent, or
a ``#'' character in the place of a digit, otherwise it is exact. In systems
with inexact numbers of varying precisions it may be useful to specify the
precision of a constant. For this purpose, numerical constants may be written
with an exponent marker that indicates the desired precision of the inexact
representation. The letters s, f, d, and l specify the use of short, single,
double, and long precision, respectively. (When fewer than four internal
inexact representations exist, the four size specifications are mapped onto
those available. For example, an implementation with two internal
representations may map short and single together and long and double
together.) In addition, the exponent marker e specifies the default precision
for the implementation. The default precision has at least as much precision as
double, but implementations may wish to allow this default to be set by the
user.

3.14159265358979F0
        Round to single --- 3.141593
0.6L0
        Extend to long --- .600000000000000

Looking at the exact schema 
(http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-10.html#%_sec_7.1.1), 
we can have:

#i#b001010010
or
#b#i001010101
or
#o1234567
-}

radix = do char '#'
           x <- oneOf "bodx"
           return "#" ++ [x]

precision = do char '#'
               x <- oneOf "ei"
               return "#" ++ [x]

prefix = do { oneOf radix; oneOf precision <|> oneOf precision; oneOf radix

parseNumber2 = do prefix <- prefix
                  return prefix

{-----------------------------
 - The following is mostly unchanged from parser2.hs
 - ---------------------------}

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
