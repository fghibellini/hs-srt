{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Lib
    ( SubRip
    , Timestamp
    , Range
    , Line
    , parse
    , RawLine
    ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString, (!?))
import Data.Word (Word8)
import Data.Word8 (isDigit, isSpace)
import Debug.Trace (traceShow, trace)
import Data.Bifunctor (bimap)

data SubRip a = SubRip [Line a]
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

data LineParseError
  = Err_01 -- expected " --> " between timestamps
  | Err_02 -- expecting newline after index
  | Err_03
  | Err_04 -- expecting newline after timestamp line
  | Err_05
  | Err_06 -- expecting digit in timestamp
  | Err_07 -- expecting ':' in timestamp
  deriving Show

parseLine :: SubRipContent a => ByteString -> Either LineParseError (Line a, ByteString)
parseLine input = do
  (index, rest0) <- parseIndex input
  (range, rest1) <- parseRange rest0
  (contts, rest2) <- parseContent rest1
  pure (Line index range contts, rest2)

lf :: ByteString
lf = "\x0A"

crlf :: ByteString
crlf = "\x0D\x0A"

newtype RawLine = RawLine ByteString
  deriving Show

class SubRipContent a where
  parseContent :: ByteString -> Either LineParseError (a, ByteString)

instance SubRipContent RawLine where
  parseContent input = do
      sepIndex <- unfold 0 \i ->
        case (i >= BS.length input, isSeparator input i) of
          (True, _) -> Left (Left Err_03)
          (False, True) -> Left (Right i)
          (False, False) -> Right (i + 1)
      let (contents, rest1) = BS.splitAt sepIndex input
      ((), rest2) <- replaceError Err_05 $ parseNewLine rest1
      ((), rest3) <- replaceError Err_05 $ parseNewLine rest2
      pure (RawLine contents, rest3)
    where
      isSeparator bs i = (isLF bs i && isLF bs (i + 1)) || (isLF bs i && isCRLF bs (i + 1)) || (isCRLF bs i && isLF bs (i + 2)) || (isCRLF bs i && isCRLF bs (i + 2))
      isLF bs i = bs !? i == Just 0x0A
      isCRLF bs i = (bs !? i == Just 0x0D) && (bs !? (i + 1) == Just 0x0A)

unfold :: a -> (a -> Either b a) -> b
unfold x f = either id (\y -> unfold y f) $ f x 

parseIndex :: ByteString -> Either LineParseError (Int, ByteString)
parseIndex input =
  let
    (digits, rest) = BS.span isDigit input
  in do
    ((), rest1) <- replaceError Err_02 $ parseNewLine rest
    pure (digitsToInt digits, rest1)


digitsToInt :: ByteString -> Int
digitsToInt = sum . zipWith (*) powersOfTen . reverse . fmap fromDigit . BS.unpack
  where
    fromDigit :: Word8 -> Int
    fromDigit b = fromIntegral (b - 48)
    powersOfTen :: [Int]
    powersOfTen = iterate (*10) 1

parseNewLine :: ByteString -> Either () ((), ByteString)
parseNewLine input = case input of
   r | BS.isPrefixOf lf r -> Right ((), BS.drop 1 input)
   r | BS.isPrefixOf crlf r -> Right ((), BS.drop 2 input)
   r | otherwise -> Left ()

parseRange :: ByteString -> Either LineParseError (Range, ByteString)
parseRange input = do
  (t0, rest0) <- parseTimestamp input
  case BS.splitAt 5 rest0 of
    (" --> ", rest1) -> do
      (t1, rest2) <- parseTimestamp rest1
      ((), rest3) <- replaceError Err_04 $ parseNewLine rest2
      pure (Range t0 t1, rest3)
    (_, _) -> Left Err_01

replaceError :: e' -> Either e a -> Either e' a
replaceError e x = mapError (\_ -> e) x 

mapError :: (e -> e') -> Either e a -> Either e' a
mapError f x = bimap f id x

data SmallErr = Eof | UnexpectedInput

parseDigit :: ByteString -> Either SmallErr (Word8, ByteString)
parseDigit input = maybe (Left Eof) (\x -> if isDigit x then Right (x, BS.drop 1 input) else Left UnexpectedInput) $ input !? 0

parseChar :: Word8 -> ByteString -> Either SmallErr ((), ByteString)
parseChar c input = maybe (Left Eof) (\x -> if x == c then Right ((), BS.drop 1 input) else Left UnexpectedInput) $ input !? 0

parseFixedDigitNumber :: Int -> ByteString -> Either SmallErr (Int, ByteString)
parseFixedDigitNumber n input = unfold 0 \i -> case i of
       _ | i >= n -> Left(Right (digitsToInt (BS.take i input), BS.drop i input))
       _ -> case input !? i of
             Just x | isDigit x -> Right(i+1)
             Just x | otherwise -> Left (Left UnexpectedInput)
             Nothing -> Left (Left Eof)

parseTimestamp :: ByteString -> Either LineParseError (Timestamp, ByteString)
parseTimestamp input = do
  (hs, rest1) <- replaceError Err_06 $ parseFixedDigitNumber 2 input
  ((), rest2) <- replaceError Err_07 $ parseChar 58 rest1 -- ':'
  (ms, rest3) <- replaceError Err_06 $ parseFixedDigitNumber 2 rest2
  ((), rest4) <- replaceError Err_07 $ parseChar 58 rest3 -- ':'
  (ss, rest5) <- replaceError Err_06 $ parseFixedDigitNumber 2 rest4
  ((), rest6) <- replaceError Err_07 $ parseChar 44 rest5 -- ','
  (mss, rest7) <- replaceError Err_06 $ parseFixedDigitNumber 3 rest6
  Right (Timestamp (hs * oneHour + ms * oneMinute + ss * oneSecond + mss), rest7)

parse :: SubRipContent a => FilePath -> IO (SubRip a)
parse f = do
  contents <- BS.drop 3 <$> BS.readFile f -- dropping BOM
  let res =
        unfold (contents,[]) \(c,ls) -> case parseLine c of
          Left e -> Left (Left e)
          Right (l, "") -> Left (Right (ls <> [l])) -- TODO append on list :(
          Right (l, r) -> Right (r, ls <> [l]) -- TODO append on list :(
  case res of
    Right lines -> pure $ SubRip lines
    Left err -> error ("Parse error: " <> show err)

