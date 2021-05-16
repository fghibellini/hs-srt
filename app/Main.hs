module Main where

import Lib (parse, SubRip, RawLine)

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> ((parse f) :: IO (SubRip RawLine)) >>= print
    xs -> do
      error ("Invalid number of arguments (expected 1, got " <> show (length xs) <> ")")
