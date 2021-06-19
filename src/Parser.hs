{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import qualified Data.ByteString as BS
import Data.Bifunctor (bimap, Bifunctor)
import Data.ByteString (ByteString, (!?))
import Data.Word (Word8)
import Debug.Trace (traceShowId, traceShow, trace)
import Control.Monad.Trans.Except (ExceptT(..), throwE, runExceptT)
import Data.ByteString.Internal (c2w)
import Data.Maybe (catMaybes)
import Data.Void (Void, absurd)


data ParserState
  = ParserState
  { position :: !Int -- current focus of the parser
  , buffer :: !ByteString -- original input data
  , len :: !Int -- length of buffer
  , filename :: !FilePath -- filename displayed in token locations
  }

newtype Parser e a = Parser { unParser :: ParserState -> Either e (a, ParserState) }

runParser :: Parser e a -> FilePath -> ByteString -> Either e a
runParser (Parser p) filename input = fst <$> p (ParserState { position = 0, buffer = input, len = BS.length input, filename = filename })

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

-- the the current focus of the parser
getPos :: Parser e Int
getPos = Parser \p@ParserState { position } -> Right (position, p)

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

parseFixedBS :: ByteString -> Parser () ()
parseFixedBS needle = do
  let len = BS.length needle
  cs <- runExceptT $ traverse (\i -> ExceptT $ (maybe (Left ()) Right <$> peekChar i)) [0..(len - 1)]
  if Right needle == (BS.pack <$> cs)
  then moveFocus len
  else failParse ()

data SmallErr = Eof | UnexpectedInput

parseChar :: Char -> Parser SmallErr ()
parseChar = parseWord8 . c2w

parseWord8 :: Word8 -> Parser SmallErr ()
parseWord8 c = do
  c' <- peekChar 0
  case c' of
    Just x | x == c -> () <$ moveFocus 1
    Just x | otherwise -> failParse UnexpectedInput
    Nothing -> failParse Eof

replaceError :: e' -> Parser e a -> Parser e' a
replaceError e x = mapError (\_ -> e) x 

mapError :: (e -> e') -> Parser e a -> Parser e' a
mapError f x = bimap f id x

-- try :: Parser e a -> Parser e a
-- try (Parser p) = Parser \s -> either ( $ p s

peek :: Parser e a -> Parser f (Maybe a)
peek (Parser p) = Parser \s -> either (\_ -> Right (Nothing, s)) (\(x, _) -> Right (Just x, s)) $ p s

alt :: Parser e1 a -> Parser e2 a -> Parser e2 a
alt (Parser p1) (Parser p2) = Parser \s -> either (\e -> p2 s) Right $ p1 s

-- broken - use parseSpan instead
parseWhile :: (Maybe Word8 -> Bool) -> Parser () ByteString
parseWhile f = do
  (i, chars) <- loopM fn (0, [])
  moveFocus i
  pure $ BS.pack $ catMaybes $ reverse chars
  where
    fn (i, cs) = do
      c <- peekChar i
      pure $ if f c
      then Left (i + 1, c:cs)
      else Right (i - 1, cs)
  
parseSpan :: (Word8 -> Bool) -> Parser Void [Word8]
parseSpan predicate = do
  r <- peekChar 0
  case r of
    Just c | predicate c -> (c :) <$> (moveFocus 1 >> parseSpan predicate)
    _ -> pure []

-- taken from https://hackage.haskell.org/package/extra-1.7.9/docs/src/Control.Monad.Extra.html#loopM
loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM act x = do
    res <- act x
    case res of
        Left x -> loopM act x
        Right v -> pure v

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

eitherToParser :: Either e a -> Parser e a
eitherToParser x = Parser \p -> (,p) <$> x

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
