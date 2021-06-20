{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib (parse, SubRip, RawLine, contents, unRawLine, Located(..))
import Lib as Lib
import Parser (Span(..), loop)
import Html (Html, innerText, printTree, innerText, InnerText(..))

import System.Environment (getArgs)
-- import Text.HTML.TagSoup (parseTags, innerText)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import Data.Foldable (for_)

removeUnwantedChars :: Text -> Text
removeUnwantedChars = T.replace ")" " " . T.replace "(" " " . T.replace "!" " " . T.replace "," " " . T.replace "\"" " " . T.replace "?" " " . T.replace "." " "

removeUnwantedWords :: [Text] -> [Text]
removeUnwantedWords = filter (/= "-")

htmlWords :: Html -> [Located Text]
htmlWords input = foldMap mapWords $ innerText input
  where
    mapWords :: Located InnerText -> [Located Text] 
    mapWords (Located spanLoc (InnerText text)) =
      let
        f (acc, i) word = (acc <> if T.all isSpace word then [] else [Located (spanLoc { start = i, end = i + T.length word - 1 }) word], i + T.length word)
        recomputeLocations :: [Text] -> [Located Text]
        recomputeLocations words = fst $ foldl f ([], start spanLoc) words
      in recomputeLocations $ sections text -- TODO compute location
    
    sections :: Text -> [Text]
    sections x = 
      let
        step :: ([Text], Text, Text) -> Either ([Text], Text, Text) [Text]
        step (acc, cur, "") = Right $ acc <> (if cur == "" then [] else [cur]) -- end
        step (acc, "", s) = Left (acc, pack [T.head s], T.tail s) -- start of span
        step (acc, cur, s) = case (isSpace $ T.last cur, isSpace $ T.head s) of
          (a, b) | a == b -> Left (acc, cur <> pack [T.head s], T.tail s)
          (_, _) | otherwise -> Left (acc <> [cur], pack [T.head s], T.tail s)
      in loop step ([], "", x)

    isSpace c = ' ' == c


parseHtmlWords :: SubRip Html -> [Located Text]
parseHtmlWords = concatMap lineWords . Lib.lines
  where
    lineWords = htmlWords . value . contents

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      error ("Invalid number of arguments (expected >0, got 0)")
    xs -> do
      pure ()
      -- subs <- ((parse f) :: IO (SubRip Html))
      -- putStrLn "DONE!"
      -- -- let
      -- --   textLines :: [Text]
      -- --   textLines = ((innerText . value . contents) <$> Lib.lines subs)
      -- --   words = removeUnwantedWords $ T.toLower <$> (T.words $ removeUnwantedChars $ T.unwords textLines)
      -- putStrLn "SUBS:"
      -- -- print subs
      -- for_ (Lib.lines subs) $ \line -> printTree (value (contents line))
      -- putStrLn "WORDS:"
      -- for_ (parseHtmlWords subs) $ \word -> do
      --    print word
      -- --putStrLn "WORDS:"
      -- --for_ words \line -> putStrLn $ unpack line

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     [f] -> do
--       subs <- ((parse f) :: IO (SubRip RawLine))
--       let
--         textLines :: [Text]
--         textLines = ((decodeUtf8 . innerText . parseTags . unRawLine . value . contents) <$> Lib.lines subs)
--         words = removeUnwantedWords $ T.toLower <$> (T.words $ removeUnwantedChars $ T.unwords textLines)
--       for_ words \line -> putStrLn $ unpack line
--     xs -> do
--       error ("Invalid number of arguments (expected 1, got " <> show (length xs) <> ")")
