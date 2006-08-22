module Main where
import System.Environment
import IO

main = do putStr "txxt: "
          hFlush stdout
          l <- getLine
          putStrLn(l)
--          putStrLn ("The sum is " ++ show (read (args !! 0)))
