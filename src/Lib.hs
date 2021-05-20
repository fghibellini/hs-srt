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
    ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString, (!?))
import Data.Word (Word8)
import Data.Word8 (isDigit, isSpace)
import Debug.Trace (traceShow, trace)
import Data.Bifunctor (bimap, Bifunctor)
import Data.Either (either)
import Data.Void (Void, absurd)
import Control.Monad.Trans.Except (ExceptT(..), throwE, runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Fail (fail)
import Data.Traversable (for)
import Debug.Trace (traceShow)

data ParserState
  = ParserState
  { position :: !Int -- current focus of the parser
  , buffer :: !ByteString -- original input data
  , len :: !Int -- length of buffer
  }

newtype Parser e a = Parser { unParser :: ParserState -> Either e (a, ParserState) }

runParser :: Parser e a -> ByteString -> Either e a
runParser (Parser p) input = fst <$> p (ParserState { position = 0, buffer = input, len = BS.length input })

instance Functor (Parser e) where
  -- fmap f (Parser p) = Parser ((fmap . fmap) f . p)
  fmap f (Parser p) = Parser \s0 -> case p s0 of
    Left e -> Left e
    Right (x, s1) -> Right (f x, s1)

instance Bifunctor Parser where
  -- bimap f1 f2 (Parser p) = Parser (bimap f1 (fmap f2) . p)
  bimap f1 f2 (Parser p) = Parser \s0 -> case p s0 of
    Left e -> Left $ f1 e
    Right (x,s1) -> Right (f2 x, s1)

instance Applicative (Parser e) where
  pure x = Parser \p -> Right (x, p)

instance Monad (Parser e) where
  Parser p1 >>= f = Parser \s0 ->
    case p1 s0 of
      Left e -> Left e
      Right (x,s1) -> let Parser p2 = f x in p2 s1

-- read character relative to focus (0 returns the focus)
peekChar :: Int -> Parser e (Maybe Word8)
peekChar i = Parser \p@ParserState { position, buffer } -> Right (buffer !? (position + i), p)

-- read bytestring
peekByteString :: Int -> Parser e ByteString
peekByteString l = Parser \p@ParserState { position, buffer } -> Right (BS.take l $ BS.drop position buffer, p)

-- shifts the focus by the passed amount (0 is a noop)
moveFocus :: Int -> Parser e ()
moveFocus n = Parser \p@ParserState { position } -> Right ((), p { position = position + n })

failParse :: e -> Parser e a
failParse e = Parser \_ -> Left e

-- for debugging purposes only
dumpState :: Parser e ()
dumpState = Parser \s -> traceShow (renderState s) (Right ((), s))
  where
    renderState (ParserState { position, buffer }) = BS.take 20 $ BS.drop position buffer

-- for debugging purposes only
parserLog :: String -> Parser e ()
parserLog msg = Parser \s -> trace msg (Right ((), s))

isDone :: Parser e Bool
isDone = Parser \p@ParserState { position, len } -> Right (position >= len, p)

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
  , contents :: a
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
  contts <- parseContent
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
          (a,_) | isLF a -> case (r2,r3) of
            (x,_) | isLF x -> True
            (x,y) | isCR x && isLF y -> True
            _ -> False
          (a,b) | isCR a && isLF b -> case (r3,r4) of
            (x,_) | isLF x -> True
            (x,y) | isCR x && isLF y -> True
            _ -> False
          _ -> False
      isLF i = i == Just 0x0A
      isCR i = i == Just 0x0D

-- same as `loop` from `extra`
unfold :: (a -> Either b a) -> a -> b
unfold f x = either id (unfold f) $ f x 

parseSpan :: (Word8 -> Bool) -> Parser Void [Word8]
parseSpan predicate = do
  r <- peekChar 0
  case r of
    Just c | predicate c -> (c :) <$> (moveFocus 1 >> parseSpan predicate)
    _ -> pure []

parseIndex :: Parser (LineParseError a) Int
parseIndex = do
  digits <- mapError absurd $ parseSpan isDigit
  replaceError Err_02 $ parseNewLine
  pure $ digitsToInt digits


digitsToInt :: [Word8] -> Int
digitsToInt = sum . zipWith (*) powersOfTen . reverse . fmap fromDigit
  where
    fromDigit :: Word8 -> Int
    fromDigit b = fromIntegral (b - 48)
    powersOfTen :: [Int]
    powersOfTen = iterate (*10) 1

parseNewLine :: Parser () ()
parseNewLine = do
  c1 <- peekChar 0
  case c1 of
    Just 0x0A -> moveFocus 1
    Just 0x0D -> do
      c2 <- peekChar 1
      case c2 of
        Just 0x0A -> moveFocus 2
        _ -> failParse ()
    _ -> failParse ()

parseFixedBS :: ByteString -> Parser () ()
parseFixedBS needle = do
  let len = BS.length needle
  cs <- runExceptT $ traverse (\i -> ExceptT $ (maybe (Left ()) Right <$> peekChar i)) [0..(len - 1)]
  if Right needle == (BS.pack <$> cs)
  then moveFocus len
  else failParse ()

parseRange :: Parser (LineParseError a) Range
parseRange = do
  t0 <- parseTimestamp
  () <- replaceError Err_01 $ parseFixedBS " --> "
  t1 <- parseTimestamp
  () <- replaceError Err_04 $ parseNewLine
  pure $ Range t0 t1

replaceError :: e' -> Parser e a -> Parser e' a
replaceError e x = mapError (\_ -> e) x 

mapError :: (e -> e') -> Parser e a -> Parser e' a
mapError f x = bimap f id x

data SmallErr = Eof | UnexpectedInput

parseChar :: Word8 -> Parser SmallErr ()
parseChar x = do
  c <- peekChar 0
  if Just x == c 
  then moveFocus 1
  else failParse UnexpectedInput
  

eitherToParser :: Either e a -> Parser e a
eitherToParser x = Parser \p -> (,p) <$> x

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
      (Just c) | isDigit c -> pure c
      (Just c) | otherwise -> throwE UnexpectedInput
      Nothing -> throwE Eof

-- flip unfold 0 \i -> case i of
--        _ | i >= n -> Left(Right (digitsToInt (BS.take i input), BS.drop i input))
--        _ -> case input !? i of
--              Just x | isDigit x -> Right(i+1)
--              Just x | otherwise -> Left (Left UnexpectedInput)
--              Nothing -> Left (Left Eof)

parseTimestamp :: Parser (LineParseError a) Timestamp
parseTimestamp = do
  hs <- replaceError Err_06 $ parseFixedDigitNumber 2
  () <- replaceError Err_07 $ parseChar 58 -- ':'
  ms <- replaceError Err_06 $ parseFixedDigitNumber 2
  () <- replaceError Err_07 $ parseChar 58 -- ':'
  ss <- replaceError Err_06 $ parseFixedDigitNumber 2
  () <- replaceError Err_08 $ parseChar 44 -- ','
  mss <- replaceError Err_06 $ parseFixedDigitNumber 3
  pure $ Timestamp (hs * oneHour + ms * oneMinute + ss * oneSecond + mss)

parse :: SubRipContent a => Show (ContentError a) => FilePath -> IO (SubRip a)
parse f = do
  contents <- BS.drop 3 <$> BS.readFile f -- dropping BOM
  case parseBS contents of
    Right subrip -> pure subrip
    Left err -> error ("Parse error: " <> show err)

parseBS :: SubRipContent a => Show (ContentError a) => ByteString -> Either (LineParseError (ContentError a)) (SubRip a)
parseBS input = do
  let parseLines = do
          l <- parseLine
          done <- isDone
          case done of
            True -> pure [l]
            False -> (l:) <$> parseLines
  case runParser parseLines input of
    Right lines -> Right $ SubRip lines
    Left err -> Left err

-- taken from https://hackage.haskell.org/package/extra-1.7.9/docs/src/Control.Monad.Extra.html#loopM
loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM act x = do
    res <- act x
    case res of
        Left x -> loopM act x
        Right v -> pure v
