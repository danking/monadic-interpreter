module Main where

import Parser
import System.Environment (getArgs)
import Run

main :: IO ()
main = do
  args <- getArgs
  case args of
    [name] -> doit name run
    ["-v",name] -> doit name run'
    _   ->
      putStrLn "Expects one argument"

doit name runner = do
  f <- readFile name
  case parse f name of
    Left err -> do
      fail $ show err
    Right v -> do
      putStrLn "Parsed:"
      putStrLn $ show v
      putStrLn "Evaluated:"
      putStrLn $ show $ runner v
      return ()
