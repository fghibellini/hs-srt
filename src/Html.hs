{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Html where

import Parser
import Lib

import Data.ByteString (ByteString)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Internal (w2c)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w)

data Html
  = TextNode ByteString
  | Element Text Html
  | Seq [Html]
  deriving (Eq, Show)

data HtmlError = HtmlError_001 deriving (Eq, Show)


innerText :: Html -> Text
innerText (TextNode bs) = decodeUtf8 bs
innerText (Element _ html) = innerText html
innerText (Seq hs) = foldMap innerText hs

parseAttributes :: Parser SmallErr [a]
parseAttributes = pure []

parseTagName :: Parser SmallErr Text
parseTagName = do
  replaceError Eof $ (decodeUtf8 . BS.pack) <$> parseSpan isIdChar -- TODO proper check
  where
    isIdChar x = w2c x `elem` idChars
    idChars = ['a'..'z'] <> ['A'..'Z']

parseHtml :: Parser (LineParseError HtmlError) Html
parseHtml = parseHtml_ [dummyRoot]
  where
    dummyRoot = HtmlOpenTag "**root**" []

data HtmlOpenTag = HtmlOpenTag { _openTagName :: Text, _childrenAccumulator :: [Html] } deriving Show

-- TODO replace dummy errors
parseHtml_ :: [HtmlOpenTag] -> Parser (LineParseError HtmlError) Html
parseHtml_ ctx = do
  parserLog $ "parseHtml"
  parserLog $ show ctx
  dumpState
  nl <- peek (parseNewLine >> parseNewLine)
  case nl of
    Just _ -> do
      _ <- replaceError (error "should never happen") $ (parseNewLine >> parseNewLine) -- TODO double work
      let ((HtmlOpenTag _ cacc):ctx') = ctx
      pure $ Seq cacc
    Nothing -> do
      c <- peekChar 0
      case c of
        Just x | x == c2w '<' -> do
          c2 <- peekChar 1
          case c2 of
            Nothing -> failParse (Err_03 HtmlError_001)
            Just x | x == c2w '/' -> parseClosingTag
            Just x | otherwise -> replaceError (Err_03 HtmlError_001) $ parseTag
        _ -> replaceError (Err_03 HtmlError_001) $ parseText []

  where
    parseText acc = do
      parserLog "parseText"
      parseText' acc

    parseText' acc = do
      let
        ((HtmlOpenTag n cacc):ctx') = ctx
        ctx'' = (HtmlOpenTag n (cacc <> [TextNode $ BS.pack $ reverse acc])):ctx'
        terminateText = parseHtml_ ctx''
      nl <- peek (parseNewLine >> parseNewLine)
      case nl of
        Just _ -> terminateText
        Nothing -> do
          c0 <- peekChar 0
          case c0 of
            Nothing -> failParse (Err_03 HtmlError_001)
            Just x | x == 60 -> terminateText
            Just x | otherwise -> moveFocus 1 >> parseText' (x:acc)

    parseTag = do
      moveFocus 1
      name <- replaceError (Err_03 HtmlError_001) parseTagName
      parserLog $ "parsed tag: " <> unpack name
      _ <- replaceError (Err_03 HtmlError_001) parseAttributes
      parserLog "parsed attributes"
      dumpState
      replaceError (Err_03 HtmlError_001) $ parseChar '>'
      parserLog "parsed '>'"
      parseHtml_ (HtmlOpenTag name [] : ctx)
      -- replaceError (Err_03 HtmlError_001) $ parseFixedBS "</"
      -- name2 <- replaceError (Err_03 HtmlError_001) parseTagName
      -- --TODO assert name == name2
      -- replaceError (Err_03 HtmlError_001) $ parseChar '>'
      -- pure $ Element name contents

    parseClosingTag = do
      moveFocus 2
      name <- replaceError (Err_03 HtmlError_001) parseTagName
      let ((HtmlOpenTag n cacc):(HtmlOpenTag n2 cacc2):ctx') = ctx
      if name /= n
      then failParse $ Err_03 HtmlError_001
      else do
        replaceError (Err_03 HtmlError_001) $ parseChar '>'
        parseHtml_ ((HtmlOpenTag n2 $ cacc2 <> [Element name (Seq cacc)]):ctx') -- TODO don't create Seq for just one element


instance SubRipContent Html where
  type ContentError Html = HtmlError
  parseContent = parseHtml

