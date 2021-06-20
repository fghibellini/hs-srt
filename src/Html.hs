{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Html where

import Parser
import Lib

import Data.ByteString (ByteString)
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Internal (w2c)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w)

data Html
  = HtmlNode (Located Node)
  | HtmlSeq [Html]
  deriving (Eq)

instance Show Html where
  show (HtmlNode (Located loc n)) = "(<" <> show n <> ">@" <> show loc <> ")"
  show (HtmlSeq xs) = foldMap show xs

data Node
  = TextNode ByteString
  | Element Text Html
  deriving (Eq, Show)

data HtmlError = HtmlError_001 deriving (Eq, Show)

data InnerText = InnerText Text deriving Show

innerText :: Html -> [Located InnerText]
innerText = go
  where
    go (HtmlNode (Located loc (TextNode bs))) = [Located loc (InnerText (decodeUtf8 bs))]
    go (HtmlNode (Located loc (Element _ html))) = go html
    go (HtmlSeq xs) = foldMap go xs

-- innerText :: Html -> Text
-- innerText = innerTextHtml
--   where
--     innerTextHtml (HtmlNode (Located _ node)) = innerTextNode node
--     innerTextHtml (HtmlSeq xs) = foldMap innerText xs
-- 
--     innerTextNode (TextNode bs) = decodeUtf8 bs
--     innerTextNode (Element _ html) = innerText html

printTree :: Html -> IO ()
printTree = printTree' ""
  where
  printTree' indent (HtmlNode (Located loc (TextNode txt))) = do
    putStr indent
    putStrLn $ "<TextNode@" <> show loc <> " text=\"" <> unpack (decodeUtf8 txt) <> "\">"
  printTree' indent (HtmlNode (Located loc (Element name children))) = do
    putStr indent
    putStrLn $ "<" <> unpack name <> "@" <> show loc <> ">"
    printTree' (indent <> "  ") children
  printTree' indent (HtmlSeq hs) = do
    putStr indent
    putStrLn $ "<--Seq-->"
    () <$ traverse (printTree' (indent <> "  ")) hs
    

parseAttributes :: Parser SmallErr [a]
parseAttributes = pure []

parseTagName :: Parser SmallErr Text
parseTagName = do
  replaceError Eof $ (decodeUtf8 . BS.pack) <$> parseSpan isIdChar -- TODO proper check
  where
    isIdChar x = w2c x `elem` idChars
    idChars = ['a'..'z'] <> ['A'..'Z']

parseHtml :: Parser (LineParseError HtmlError) Html
parseHtml = do
    i <- getPos
    parseHtml_ [dummyRoot i]
  where
    dummyRoot i = HtmlOpenTag "**root**" i []

data HtmlOpenTag = HtmlOpenTag { _openTagName :: Text, _startOffset :: Int, _childrenAccumulator :: [Html] } deriving Show

-- TODO replace dummy errors
parseHtml_ :: [HtmlOpenTag] -> Parser (LineParseError HtmlError) Html
parseHtml_ ctx = do
  parserLog $ "parseHtml"
  -- parserLog $ show ctx
  dumpState
  nl <- peek (parseNewLine >> parseNewLine)
  case nl of
    Just _ -> do
      _ <- replaceError (error "should never happen") $ (parseNewLine >> parseNewLine) -- TODO double work
      let ((HtmlOpenTag _ i cacc):ctx') = ctx
      span <- genSpan
      pure $ HtmlSeq cacc
    Nothing -> do
      c <- peekChar 0
      case c of
        C x | x == c2w '<' -> do
          c2 <- peekChar 1
          case c2 of
            EOI -> failParse (Err_03 HtmlError_001)
            C x | x == c2w '/' -> parseClosingTag
            C x | otherwise -> replaceError (Err_03 HtmlError_001) $ parseTag
        _ -> replaceError (Err_03 HtmlError_001) parseText

  where
    parseText = do
      i0 <- getPos
      let
        parseText' acc = do
          let
            ((HtmlOpenTag n i cacc):ctx') = ctx
            ctx'' :: Span -> [HtmlOpenTag]
            ctx'' span = (HtmlOpenTag n i (cacc <> [HtmlNode $ Located (span { start = i0 }) $ TextNode $ BS.pack $ reverse acc])):ctx'
            terminateText = do
              span <- genSpan
              parseHtml_ (ctx'' span)
          nl <- peek (parseNewLine >> parseNewLine)
          case nl of
            Just _ -> terminateText
            Nothing -> do
              c0 <- peekChar 0
              case c0 of
                EOI -> failParse (Err_03 HtmlError_001)
                C 60 -> terminateText
                C x -> moveFocus 1 >> parseText' (x:acc)
      parserLog "parseText"
      parseText' []

    parseTag = do
      sOffset <- getPos
      moveFocus 1
      name <- replaceError (Err_03 HtmlError_001) parseTagName
      parserLog $ "parsed tag: " <> unpack name
      _ <- replaceError (Err_03 HtmlError_001) parseAttributes
      parserLog "parsed attributes"
      dumpState
      replaceError (Err_03 HtmlError_001) $ parseChar '>'
      parserLog "parsed '>'"
      parseHtml_ (HtmlOpenTag name sOffset [] : ctx)
      -- replaceError (Err_03 HtmlError_001) $ parseFixedBS "</"
      -- name2 <- replaceError (Err_03 HtmlError_001) parseTagName
      -- --TODO assert name == name2
      -- replaceError (Err_03 HtmlError_001) $ parseChar '>'
      -- pure $ Element name contents

    parseClosingTag = do
      moveFocus 2
      name <- replaceError (Err_03 HtmlError_001) parseTagName
      let ((HtmlOpenTag n i cacc):(HtmlOpenTag n2 i2 cacc2):ctx') = ctx
      if name /= n
      then failParse $ Err_03 HtmlError_001
      else do
        span <- genSpan
        replaceError (Err_03 HtmlError_001) $ parseChar '>'
        parseHtml_ ((HtmlOpenTag n2 i2 $ cacc2 <> [HtmlNode $ Located (span { start =  i }) $ Element name (HtmlSeq cacc)]):ctx') -- TODO don't create Seq for just one element, TODO fix location of Seq


instance SubRipContent Html where
  type ContentError Html = HtmlError
  parseContent = parseHtml

