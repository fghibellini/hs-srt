{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Criterion.Main
import Lib (parse, parseBS, SubRip, RawLine, contents, unRawLine)
import Lib as Lib
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

example1k, example10k, example100k :: String
example1k =  "./data/s01e03-1k.srt"
example10k =  "./data/s01e04-10k.srt"
example100k =  "./data/s01e05-100k.srt"

benchParseFile :: String -> Benchmark
benchParseFile f = bench ("bench parseFile " <> f) $ whnfIO do
  subs <- ((parse f) :: IO (SubRip RawLine))
  pure subs

benchParseBS :: String -> Benchmark
benchParseBS f = env prepareInput $ \input -> bench ("bench parse " <> f) $ whnf (parseBS  :: ByteString -> Either _ (SubRip RawLine)) input
  where
    prepareInput :: IO ByteString
    prepareInput = BS.readFile f
  
main = defaultMain
  [ bgroup "File parsing"
    [ benchParseFile example1k
    , benchParseFile example10k
    , benchParseFile example100k
    ]
  , bgroup "ByteString parsing"
    [ benchParseBS example1k
    , benchParseBS example10k
    , benchParseBS example100k
    ]
  ]
