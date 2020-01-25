module Main where

import Text.Read
import System.Environment
import System.IO

import CGT.Parser
import CGT.Values

main :: IO ()
main = do
  args <- getArgs
  let n = case args of
            [] -> number 0
            s:_ -> valueParser s
  putStrLn $ show n
