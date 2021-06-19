{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib (parse, SubRip, RawLine, contents, unRawLine, Located(..))
import Lib as Lib

import System.Environment (getArgs)
import Text.HTML.TagSoup (parseTags, innerText)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Foldable (for_)

removeUnwantedChars :: Text -> Text
removeUnwantedChars = T.replace ")" " " . T.replace "(" " " . T.replace "!" " " . T.replace "," " " . T.replace "\"" " " . T.replace "?" " " . T.replace "." " "

removeUnwantedWords :: [Text] -> [Text]
removeUnwantedWords = filter (/= "-")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> do
      subs <- ((parse f) :: IO (SubRip RawLine))
      let
        textLines :: [Text]
        textLines = ((decodeUtf8 . innerText . parseTags . unRawLine . value . contents) <$> Lib.lines subs)
        words = removeUnwantedWords $ T.toLower <$> (T.words $ removeUnwantedChars $ T.unwords textLines)
      for_ words \line -> putStrLn $ unpack line
    xs -> do
      error ("Invalid number of arguments (expected 1, got " <> show (length xs) <> ")")
