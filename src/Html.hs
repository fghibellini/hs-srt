{-# LANGUAGE OverloadedStrings #-}

module Html where

import Parser

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Internal (w2c)

data Html
  = TextNode ByteString
  | Element Text Html
  | Seq [Html]

data HtmlError = HtmlError_001



parseAttributes :: Parser SmallErr [a]
parseAttributes = pure []

parseTagName :: Parser SmallErr Text
parseTagName = do
  replaceError Eof $ decodeUtf8 <$> parseWhile isIdChar -- TODO proper check
  where
    isIdChar (Just x) = w2c x `elem` idChars
    idChars = ['a'..'z'] <> ['A'..'Z']

parseHtml :: Parser HtmlError Html
parseHtml = do
  c <- peekChar 0
  case c of
    Just 60 -> replaceError HtmlError_001 $ parseTag -- '<'
    _ -> replaceError HtmlError_001 $ parseText

  where
    parseText = do
      pure $ TextNode ""
    parseTag = do
      moveFocus 1
      name <- parseTagName
      _ <- parseAttributes
      parseChar '>'
      -- contents <- parseHtml_ []
      let contents = TextNode ""
      parseChar '<'
      parseChar '/'
      name2 <- parseTagName
      --TODO assert name == name2
      parseChar '>'
      pure $ Element name contents
