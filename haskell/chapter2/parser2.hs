module Main where
import System.Environment
import IO
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad

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

{- many1 == + (one or more operator)
 '.' is the function composition operator. Number is our constructor from
 LispVal, and read converts a string to a number, so we have:
   Number(read(digit+))
 The tutorial says that liftM tells Number . read "to just operate on the 
 value inside the monad, giving us back a Parser LispVal", instead of a Parser
 String (huh?). liftM is in the Monad module.
-}
parseNumber = liftM (Number . read) $ (many1 digit)

parseExpr = parseAtom <|> parseString <|> parseNumber

--args to parse function are: parser name-of-parser text-to-parse
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main = do args <- getArgs
          putStrLn (args !! 0)
          putStrLn (readExpr (args !! 0))
