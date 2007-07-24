module Main where
import System.Environment
import IO
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad

{- Exercises 2 & 3: Our strings aren't quite R5RS compliant, because they don't support escaping of 
 - internal quotes 
 - within the string. Change parseString so that \" gives a literal quote character instead of 
 - terminating the string. You may want to replace noneOf "\"" with a new parser action that accepts 
 - either a non-quote character or a backslash followed by a quote mark.
 - 3: Modify the previous exercise to support \n, \r, \t, \\, and any other desired escape characters
 -}

escapes = oneOf "\\\""

escapedChar = do char '\\'
                 x <- escapes
                 return [x]

escapeSeqs = oneOf "rnt"

escapeSeq = do a <- char '\\'
               b <- escapeSeqs
               return $ [a] ++ [b]

-- [^"]+ -> [^"]+ | \\"
-- x <- many (noneOf "\"")
-- x <- validStringChars
parseString = do char '"'
                 x <- many (many1 (noneOf "\"\\") <|> escapeSeq <|> escapedChar)
                 char '"'
                 return $ String $ concat x

--
--
-- What follows is parser2; concatenated mostly unchanged
-- 
-- 

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol = oneOf "!$#%&|*+-/:<=>?@^_~"
spaces = skipMany1 space

parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

parseNumber = liftM (Number . read) $ (many1 digit)

parseExpr = parseAtom <|> parseString <|> parseNumber

--args to parse function are: parser name-of-parser text-to-parse
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main = do args <- getArgs
          putStrLn (args !! 0)
          putStrLn (readExpr (args !! 0))
