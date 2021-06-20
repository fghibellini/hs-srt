{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Lib
    ( SubRip
    , Timestamp
    , Range
    , Line
    , parse
    , parseBS
    , RawLine(..)
    , Lib.lines
    , contents
    , Located(..)
    , LineParseError(..)
    , SubRipContent(..)
    ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString, (!?))
import Data.Word (Word8)
import Data.Word8 (isDigit, isSpace)
import Debug.Trace (traceShowId, traceShow, trace)
import Data.Either (either)
import Data.Void (Void, absurd)
import Control.Monad.Trans.Except (ExceptT(..), throwE, runExceptT)
import Data.Bifunctor (bimap, Bifunctor)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Fail (fail)
import Data.Traversable (for)
import Parser

data SubRip a = SubRip { lines :: [Line a] }
  deriving Show

newtype Timestamp = Timestamp Int

-- TODO create functions that use Text instead of String
instance Show Timestamp where
  show (Timestamp t) = z2 hs <> ":" <> z2 ms <> ":" <> z2 ss <> "," <> z3 mss
    where
      z2 n | n < 10 = "0" <> show n
      z2 n = show n
      z3 n | n < 10 = "00" <> show n
      z3 n | n < 100 = "0" <> show n
      z3 n = show n
      hs = t `div` oneHour
      ms = (t - hs * oneHour) `div` oneMinute
      ss = (t - hs * oneHour - ms * oneMinute) `div` oneSecond
      mss = t `mod` oneSecond

oneHour = 60 * 60 * 1000
oneMinute = 60 * 1000
oneSecond = 1000

data Range = Range
  { from :: Timestamp
  , to :: Timestamp
  }

instance Show Range where
  show (Range from to) = show from <> " --> " <> show to 

data Line a
  = Line
  { index :: Int
  , range :: Range
  , contents :: Located a
  }

instance Show a => Show (Line a) where
  show line = unlines
    [ show $ index line
    , show $ range line
    , show $ contents line
    ]

data LineParseError a
  = Err_01 -- expected " --> " between timestamps
  | Err_02 -- expecting newline after index
  | Err_03 a
  | Err_04 -- expecting newline after timestamp line
  | Err_06 -- expecting digit in timestamp
  | Err_07 -- expecting ':' in timestamp
  | Err_08 -- expecting ',' in timestamp
  deriving Show

data RawLineParsingError
  = RawErr_01 -- expecting double newline after content
  | RawErr_02 -- this error should never happen (we consume two newlines, but we know they're there since that's why we stoped consuming content data)
  deriving Show

parseLine :: SubRipContent a => Parser (LineParseError (ContentError a)) (Line a)
parseLine = do
  index <- parseIndex
  range <- parseRange 
  contts <- parseLocated parseContent
  pure $ Line index range contts

lf :: ByteString
lf = "\x0A"

crlf :: ByteString
crlf = "\x0D\x0A"


newtype RawLine = RawLine { unRawLine :: ByteString }
  deriving Show

class SubRipContent a where
  type ContentError a :: *
  parseContent :: Parser (LineParseError (ContentError a)) a

parseLocated :: Parser e a -> Parser e (Located a)
parseLocated p = do
      span <- genSpan
      p0 <- getPos
      v <- p
      p1 <- getPos
      pure $ Located { location = span { start = p0, end = p1 }, value = v }

instance SubRipContent RawLine where
  type ContentError RawLine = RawLineParsingError
  parseContent = do
      sepIndex <- flip loopM 0 \i -> do
        r <- isSeparator i
        case r of
          True -> pure (Right i)
          False -> pure (Left (i+1))
      contents <- peekByteString sepIndex
      moveFocus sepIndex
      () <- replaceError (Err_03 RawErr_02) $ parseNewLine
      () <- replaceError (Err_03 RawErr_02) $ parseNewLine
      pure $ RawLine contents
    where
      isSeparator i = do 
        r1 <- peekChar (i + 0)
        r2 <- peekChar (i + 1)
        r3 <- peekChar (i + 2)
        r4 <- peekChar (i + 3)
        pure $ case (r1,r2) of
          (LF,_) -> case (r2,r3) of
            (LF,_) -> True
            (CR,LF) -> True
            _ -> False
          (CR,LF) -> case (r3,r4) of
            (LF,_) -> True
            (CR,LF) -> True
            _ -> False
          _ -> False

-- same as `loop` from `extra`
unfold :: (a -> Either b a) -> a -> b
unfold f x = either id (unfold f) $ f x 

parseIndex :: Parser (LineParseError a) Int
parseIndex = do
  digits <- mapError absurd $ parseSpan isDigit
  dumpState
  replaceError Err_02 parseNewLine
  pure $ digitsToInt digits


digitsToInt :: [Word8] -> Int
digitsToInt = sum . zipWith (*) powersOfTen . reverse . fmap fromDigit
  where
    fromDigit :: Word8 -> Int
    fromDigit b = fromIntegral (b - 48)
    powersOfTen :: [Int]
    powersOfTen = iterate (*10) 1



-- parseChar :: Word8 -> Parser SmallErr ()
-- parseChar x = do
--   c <- peekChar 0
--   if Just x == c 
--   then moveFocus 1
--   else failParse UnexpectedInput
  

parseFixedDigitNumber :: Int -> Parser SmallErr Int
parseFixedDigitNumber n = do
    r <- parseDigits
    case r of
      Left e -> failParse e
      Right digits -> do
        moveFocus n
        pure $ digitsToInt digits
  where
    parseDigits :: Parser a (Either SmallErr [Word8])
    parseDigits = runExceptT $ for [0..(n-1)] \i -> lift (peekChar i) >>= \case 
      C c | isDigit c -> pure c
      C c | otherwise -> throwE UnexpectedInput
      EOI -> throwE Eof

-- flip unfold 0 \i -> case i of
--        _ | i >= n -> Left(Right (digitsToInt (BS.take i input), BS.drop i input))
--        _ -> case input !? i of
--              Just x | isDigit x -> Right(i+1)
--              Just x | otherwise -> Left (Left UnexpectedInput)
--              Nothing -> Left (Left Eof)

parseTimestamp :: Parser (LineParseError a) Timestamp
parseTimestamp = do
  hs <- replaceError Err_06 $ parseFixedDigitNumber 2
  () <- replaceError Err_07 $ parseWord8 58 -- ':'
  ms <- replaceError Err_06 $ parseFixedDigitNumber 2
  () <- replaceError Err_07 $ parseWord8 58 -- ':'
  ss <- replaceError Err_06 $ parseFixedDigitNumber 2
  () <- replaceError Err_08 $ parseWord8 44 -- ','
  mss <- replaceError Err_06 $ parseFixedDigitNumber 3
  pure $ Timestamp (hs * oneHour + ms * oneMinute + ss * oneSecond + mss)

parse :: SubRipContent a => Show (ContentError a) => FilePath -> IO (SubRip a)
parse f = do
  -- TODO perf ?
  contents <- BS.readFile f <&> \x -> case BS.take 3 x of -- dropping BOM
    "\239\187\191" -> BS.drop 3 x
    _ -> x
  case parseData f contents of
    Right subrip -> pure subrip
    Left err -> error ("Parse error: " <> show err)

parseRange :: Parser (LineParseError a) Range
parseRange = do
  t0 <- parseTimestamp
  () <- replaceError Err_01 $ parseFixedBS " --> "
  t1 <- parseTimestamp
  () <- replaceError Err_04 $ parseNewLine
  pure $ Range t0 t1

parseBS :: SubRipContent a => Show (ContentError a) => ByteString -> Either (LineParseError (ContentError a)) (SubRip a)
parseBS input = parseData "<bytestring>" input

parseData :: SubRipContent a => Show (ContentError a) => FilePath -> ByteString -> Either (LineParseError (ContentError a)) (SubRip a)
parseData sourceName input = do
  let parseLines = do
          l <- parseLine
          done <- isDone
          case done of
            True -> pure [l]
            False -> (l:) <$> parseLines
  case runParser parseLines sourceName input of
    Right lines -> Right $ SubRip lines
    Left err -> Left err

