module Main where

import Parser
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [name] ->
      do f <- readFile name
         case parse f name of
           Left err -> do
             fail $ show err
           Right v -> do
             putStrLn "Parsed: "
             putStrLn $ show v
             return ()
    _   ->
      putStrLn "Expects one argument"
